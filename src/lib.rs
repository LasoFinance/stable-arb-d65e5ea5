//! stable-arb: a zero-capital, atomic arbitrage between two stablecoin pools.
//!
//! Two pools hold the same stablecoin pair but price it with different curves:
//!   kind 0 = constant product (x*y=k), kind 1 = Curve-style stable swap (amplified).
//! When the pools disagree on the peg, `arb` finds the profit-maximising trade size
//! on-chain, then runs it as a flash route: it pulls the cheap token out of pool P,
//! sells it into pool Q for the base token, and repays pool P, all in one instruction.
//! The caller needs no starting balance and keeps whatever is left over. If there is
//! no profit above `min_profit`, the whole transaction reverts.
use anchor_lang::prelude::*;
use anchor_spl::associated_token::AssociatedToken;
use anchor_spl::token::{self, Mint, MintTo, Token, TokenAccount, Transfer};

declare_id!("FpcT2YuU9sJZDYrQDAiEyTBkgju5ssk8nsK9PeqdMspA");

pub const KIND_CP: u8 = 0;
pub const KIND_STABLE: u8 = 1;
pub const MAX_RESERVE: u64 = 1_000_000_000_000; // 1M tokens at 6 decimals
pub const FAUCET_CAP: u64 = 10_000_000_000; // 10k tokens per mint per call

// ---------------------------------------------------------------- math

fn stable_d(x: u128, y: u128, amp: u128) -> Option<u128> {
    if x == 0 || y == 0 {
        return None;
    }
    let s = x.checked_add(y)?;
    let ann = amp.checked_mul(2)?;
    let mut d = s;
    for _ in 0..64 {
        let mut dp = d;
        dp = dp.checked_mul(d)? / x.checked_mul(2)?;
        dp = dp.checked_mul(d)? / y.checked_mul(2)?;
        let prev = d;
        let num = ann
            .checked_mul(s)?
            .checked_add(dp.checked_mul(2)?)?
            .checked_mul(d)?;
        let den = ann
            .checked_sub(1)?
            .checked_mul(d)?
            .checked_add(dp.checked_mul(3)?)?;
        d = num / den;
        if d.abs_diff(prev) <= 1 {
            return Some(d);
        }
    }
    None
}

fn stable_y(x_new: u128, d: u128, amp: u128) -> Option<u128> {
    if x_new == 0 {
        return None;
    }
    let ann = amp.checked_mul(2)?;
    let c = d.checked_mul(d)? / x_new.checked_mul(2)?;
    let c = c.checked_mul(d)? / ann.checked_mul(2)?;
    let b = x_new.checked_add(d / ann)?;
    let mut y = d;
    for _ in 0..64 {
        let prev = y;
        let den = y.checked_mul(2)?.checked_add(b)?.checked_sub(d)?;
        y = y.checked_mul(y)?.checked_add(c)? / den;
        if y.abs_diff(prev) <= 1 {
            return Some(y);
        }
    }
    None
}

/// A pool's pricing curve frozen at its current reserves.
pub struct Curve {
    kind: u8,
    amp: u128,
    fee_bps: u128,
    ra: u128,
    rb: u128,
    d: u128,
}

impl Curve {
    pub fn new(pool: &Pool, ra: u64, rb: u64) -> Option<Curve> {
        if ra == 0 || rb == 0 {
            return None;
        }
        let d = if pool.kind == KIND_STABLE {
            stable_d(ra as u128, rb as u128, pool.amp as u128)?
        } else {
            0
        };
        Some(Curve { kind: pool.kind, amp: pool.amp as u128, fee_bps: pool.fee_bps as u128, ra: ra as u128, rb: rb as u128, d })
    }

    /// Tokens received for `amount_in` (fee taken on the input side).
    pub fn out(&self, a_to_b: bool, amount_in: u64) -> Option<u64> {
        let (rin, rout) = if a_to_b { (self.ra, self.rb) } else { (self.rb, self.ra) };
        let net = (amount_in as u128).checked_mul(10_000 - self.fee_bps)? / 10_000;
        if net == 0 {
            return None;
        }
        let out = if self.kind == KIND_CP {
            rout.checked_mul(net)? / rin.checked_add(net)?
        } else {
            let y_new = stable_y(rin.checked_add(net)?, self.d, self.amp)?;
            rout.checked_sub(y_new)?.checked_sub(1)?
        };
        if out == 0 || out >= rout {
            return None;
        }
        u64::try_from(out).ok()
    }
}

/// Profit (in token A) of: take B out of `p` for `x` of A, sell that B into `q` for A.
fn flash_profit(p: &Curve, q: &Curve, x: u64) -> i128 {
    let run = || -> Option<i128> {
        let m = p.out(true, x)?;
        let y = q.out(false, m)?;
        Some(y as i128 - x as i128)
    };
    run().unwrap_or(i128::MIN)
}

// ---------------------------------------------------------------- program

#[program]
pub mod stable_arb {
    use super::*;

    /// One-time: create the two test stablecoins (tUSDC, tUSDT) and the faucet authority.
    pub fn init_mints(_ctx: Context<InitMints>) -> Result<()> {
        Ok(())
    }

    /// Anyone can mint test stablecoins to themselves (devnet only).
    pub fn faucet(ctx: Context<Faucet>, amount_a: u64, amount_b: u64) -> Result<()> {
        require!(amount_a <= FAUCET_CAP && amount_b <= FAUCET_CAP, ArbError::FaucetCap);
        let seeds: &[&[u8]] = &[b"faucet", &[ctx.bumps.faucet]];
        for (mint, to, amt) in [
            (&ctx.accounts.mint_a, &ctx.accounts.user_a, amount_a),
            (&ctx.accounts.mint_b, &ctx.accounts.user_b, amount_b),
        ] {
            if amt > 0 {
                token::mint_to(
                    CpiContext::new_with_signer(
                        ctx.accounts.token_program.key(),
                        MintTo { mint: mint.to_account_info(), to: to.to_account_info(), authority: ctx.accounts.faucet.to_account_info() },
                        &[seeds],
                    ),
                    amt,
                )?;
            }
        }
        Ok(())
    }

    /// Create a pool for the pair. kind 0 = constant product, kind 1 = stable swap.
    pub fn create_pool(ctx: Context<CreatePool>, kind: u8, amp: u64, fee_bps: u16) -> Result<()> {
        require!(kind <= KIND_STABLE, ArbError::BadParams);
        require!(fee_bps <= 100, ArbError::BadParams);
        require!(kind == KIND_CP || (1..=10_000).contains(&amp), ArbError::BadParams);
        require!(ctx.accounts.mint_a.key() != ctx.accounts.mint_b.key(), ArbError::BadParams);
        require!(ctx.accounts.mint_a.decimals == ctx.accounts.mint_b.decimals, ArbError::BadParams);
        let p = &mut ctx.accounts.pool;
        p.authority = ctx.accounts.authority.key();
        p.mint_a = ctx.accounts.mint_a.key();
        p.mint_b = ctx.accounts.mint_b.key();
        p.vault_a = ctx.accounts.vault_a.key();
        p.vault_b = ctx.accounts.vault_b.key();
        p.kind = kind;
        p.amp = amp;
        p.fee_bps = fee_bps;
        p.bump = ctx.bumps.pool;
        Ok(())
    }

    /// Pool owner seeds liquidity (no LP shares: this is a small demo).
    pub fn add_liquidity(ctx: Context<AddLiquidity>, amount_a: u64, amount_b: u64) -> Result<()> {
        let a = &ctx.accounts;
        for (from, to, amt) in [(&a.owner_a, &a.vault_a, amount_a), (&a.owner_b, &a.vault_b, amount_b)] {
            if amt > 0 {
                require!(to.amount.checked_add(amt).ok_or(ArbError::Math)? <= MAX_RESERVE, ArbError::ReserveCap);
                token::transfer(
                    CpiContext::new(
                        a.token_program.key(),
                        Transfer { from: from.to_account_info(), to: to.to_account_info(), authority: a.owner.to_account_info() },
                    ),
                    amt,
                )?;
            }
        }
        Ok(())
    }

    /// Plain swap against one pool.
    pub fn swap(ctx: Context<Swap>, a_to_b: bool, amount_in: u64, min_out: u64) -> Result<()> {
        let a = &ctx.accounts;
        let curve = Curve::new(&a.pool, a.vault_a.amount, a.vault_b.amount).ok_or(ArbError::Math)?;
        let out = curve.out(a_to_b, amount_in).ok_or(ArbError::Math)?;
        require!(out >= min_out, ArbError::Slippage);
        let (uin, vin, vout, uout) = if a_to_b {
            (&a.user_a, &a.vault_a, &a.vault_b, &a.user_b)
        } else {
            (&a.user_b, &a.vault_b, &a.vault_a, &a.user_a)
        };
        require!(vin.amount.checked_add(amount_in).ok_or(ArbError::Math)? <= MAX_RESERVE, ArbError::ReserveCap);
        let tp = a.token_program.key();
        user_pay(tp, uin, vin, &a.user, amount_in)?;
        pool_pay(tp, &a.pool, vout, uout, out)?;
        Ok(())
    }

    /// Zero-capital atomic arbitrage between two pools of the same pair.
    /// Tries both directions at `size` (the best size is found off-chain), runs the
    /// better one as a flash route and reverts unless the caller nets >= `min_profit` of token A.
    pub fn arb(ctx: Context<Arb>, size: u64, min_profit: u64) -> Result<()> {
        let a = &ctx.accounts;
        require!(a.pool_x.key() != a.pool_y.key(), ArbError::BadParams);
        require!(a.pool_x.mint_a == a.pool_y.mint_a && a.pool_x.mint_b == a.pool_y.mint_b, ArbError::BadParams);
        require!(size > 0, ArbError::BadParams);
        let cx = Curve::new(&a.pool_x, a.x_a.amount, a.x_b.amount).ok_or(ArbError::Math)?;
        let cy = Curve::new(&a.pool_y, a.y_a.amount, a.y_b.amount).ok_or(ArbError::Math)?;

        // Direction: whichever pool sells B cheaper (per the curves) is the one we buy from.
        let (px, py) = (flash_profit(&cx, &cy, size), flash_profit(&cy, &cx, size));
        let x_first = px >= py;
        let (p, q) = if x_first { (&cx, &cy) } else { (&cy, &cx) };
        let profit = px.max(py);
        require!(profit > 0 && profit >= min_profit.max(1) as i128, ArbError::NoArb);
        let m = p.out(true, size).ok_or(ArbError::Math)?; // B pulled out of P
        let y = q.out(false, m).ok_or(ArbError::Math)?; // A received from Q

        let (pp, pva, pvb, qp, qva, qvb) = if x_first {
            (&a.pool_x, &a.x_a, &a.x_b, &a.pool_y, &a.y_a, &a.y_b)
        } else {
            (&a.pool_y, &a.y_a, &a.y_b, &a.pool_x, &a.x_a, &a.x_b)
        };
        require!(qvb.amount.checked_add(m).ok_or(ArbError::Math)? <= MAX_RESERVE, ArbError::ReserveCap);
        require!(pva.amount.checked_add(size).ok_or(ArbError::Math)? <= MAX_RESERVE, ArbError::ReserveCap);

        let before = a.trader_a.amount;
        let tp = a.token_program.key();
        pool_pay(tp, pp, pvb, &a.trader_b, m)?; // 1. flash: take B out of P
        user_pay(tp, &a.trader_b, qvb, &a.trader, m)?; // 2. sell B into Q ...
        pool_pay(tp, qp, qva, &a.trader_a, y)?; //        ... receive A
        user_pay(tp, &a.trader_a, pva, &a.trader, size)?; // 3. repay P in A

        let ta = &mut ctx.accounts.trader_a;
        ta.reload()?;
        let gained = ta.amount.checked_sub(before).ok_or(ArbError::NoArb)?;
        require!(gained >= min_profit.max(1), ArbError::NoArb);

        let s = &mut ctx.accounts.stats;
        s.total_arbs = s.total_arbs.saturating_add(1);
        s.total_profit = s.total_profit.saturating_add(gained);
        s.last_profit = gained;
        s.last_trader = ctx.accounts.trader.key();
        msg!("arb: size {} A, pulled {} B, got {} A, profit {} A", size, m, y, gained);
        Ok(())
    }
}

fn user_pay<'info>(
    tp: Pubkey,
    from: &Account<'info, TokenAccount>,
    to: &Account<'info, TokenAccount>,
    signer: &Signer<'info>,
    amount: u64,
) -> Result<()> {
    token::transfer(
        CpiContext::new(tp, Transfer { from: from.to_account_info(), to: to.to_account_info(), authority: signer.to_account_info() }),
        amount,
    )
}

fn pool_pay<'info>(
    tp: Pubkey,
    pool: &Account<'info, Pool>,
    from: &Account<'info, TokenAccount>,
    to: &Account<'info, TokenAccount>,
    amount: u64,
) -> Result<()> {
    let seeds: &[&[u8]] = &[b"pool", pool.mint_a.as_ref(), pool.mint_b.as_ref(), &[pool.kind], &[pool.bump]];
    token::transfer(
        CpiContext::new_with_signer(
            tp,
            Transfer { from: from.to_account_info(), to: to.to_account_info(), authority: pool.to_account_info() },
            &[seeds],
        ),
        amount,
    )
}

// ---------------------------------------------------------------- accounts

#[derive(Accounts)]
pub struct InitMints<'info> {
    #[account(mut)]
    pub payer: Signer<'info>,
    /// CHECK: PDA that is the mint authority of both test tokens.
    #[account(seeds = [b"faucet"], bump)]
    pub faucet: UncheckedAccount<'info>,
    #[account(init, payer = payer, seeds = [b"mint_a"], bump, mint::decimals = 6, mint::authority = faucet)]
    pub mint_a: Box<Account<'info, Mint>>,
    #[account(init, payer = payer, seeds = [b"mint_b"], bump, mint::decimals = 6, mint::authority = faucet)]
    pub mint_b: Box<Account<'info, Mint>>,
    pub token_program: Program<'info, Token>,
    pub system_program: Program<'info, System>,
}

#[derive(Accounts)]
pub struct Faucet<'info> {
    #[account(mut)]
    pub user: Signer<'info>,
    /// CHECK: PDA mint authority.
    #[account(seeds = [b"faucet"], bump)]
    pub faucet: UncheckedAccount<'info>,
    #[account(mut, seeds = [b"mint_a"], bump)]
    pub mint_a: Box<Account<'info, Mint>>,
    #[account(mut, seeds = [b"mint_b"], bump)]
    pub mint_b: Box<Account<'info, Mint>>,
    #[account(init_if_needed, payer = user, associated_token::mint = mint_a, associated_token::authority = user)]
    pub user_a: Box<Account<'info, TokenAccount>>,
    #[account(init_if_needed, payer = user, associated_token::mint = mint_b, associated_token::authority = user)]
    pub user_b: Box<Account<'info, TokenAccount>>,
    pub token_program: Program<'info, Token>,
    pub associated_token_program: Program<'info, AssociatedToken>,
    pub system_program: Program<'info, System>,
}

#[derive(Accounts)]
#[instruction(kind: u8)]
pub struct CreatePool<'info> {
    #[account(mut)]
    pub authority: Signer<'info>,
    pub mint_a: Box<Account<'info, Mint>>,
    pub mint_b: Box<Account<'info, Mint>>,
    #[account(
        init, payer = authority, space = 8 + 32 * 5 + 1 + 8 + 2 + 1,
        seeds = [b"pool", mint_a.key().as_ref(), mint_b.key().as_ref(), &[kind]], bump
    )]
    pub pool: Box<Account<'info, Pool>>,
    #[account(init, payer = authority, seeds = [b"vault_a", pool.key().as_ref()], bump, token::mint = mint_a, token::authority = pool)]
    pub vault_a: Box<Account<'info, TokenAccount>>,
    #[account(init, payer = authority, seeds = [b"vault_b", pool.key().as_ref()], bump, token::mint = mint_b, token::authority = pool)]
    pub vault_b: Box<Account<'info, TokenAccount>>,
    pub token_program: Program<'info, Token>,
    pub system_program: Program<'info, System>,
}

#[derive(Accounts)]
pub struct AddLiquidity<'info> {
    #[account(mut, address = pool.authority)]
    pub owner: Signer<'info>,
    pub pool: Box<Account<'info, Pool>>,
    #[account(mut, address = pool.vault_a)]
    pub vault_a: Box<Account<'info, TokenAccount>>,
    #[account(mut, address = pool.vault_b)]
    pub vault_b: Box<Account<'info, TokenAccount>>,
    #[account(mut, token::mint = pool.mint_a, token::authority = owner)]
    pub owner_a: Box<Account<'info, TokenAccount>>,
    #[account(mut, token::mint = pool.mint_b, token::authority = owner)]
    pub owner_b: Box<Account<'info, TokenAccount>>,
    pub token_program: Program<'info, Token>,
}

#[derive(Accounts)]
pub struct Swap<'info> {
    pub user: Signer<'info>,
    pub pool: Box<Account<'info, Pool>>,
    #[account(mut, address = pool.vault_a)]
    pub vault_a: Box<Account<'info, TokenAccount>>,
    #[account(mut, address = pool.vault_b)]
    pub vault_b: Box<Account<'info, TokenAccount>>,
    #[account(mut, token::mint = pool.mint_a, token::authority = user)]
    pub user_a: Box<Account<'info, TokenAccount>>,
    #[account(mut, token::mint = pool.mint_b, token::authority = user)]
    pub user_b: Box<Account<'info, TokenAccount>>,
    pub token_program: Program<'info, Token>,
}

#[derive(Accounts)]
pub struct Arb<'info> {
    #[account(mut)]
    pub trader: Signer<'info>,
    #[account(address = pool_x.mint_a)]
    pub mint_a: Box<Account<'info, Mint>>,
    #[account(address = pool_x.mint_b)]
    pub mint_b: Box<Account<'info, Mint>>,
    #[account(init_if_needed, payer = trader, associated_token::mint = mint_a, associated_token::authority = trader)]
    pub trader_a: Box<Account<'info, TokenAccount>>,
    #[account(init_if_needed, payer = trader, associated_token::mint = mint_b, associated_token::authority = trader)]
    pub trader_b: Box<Account<'info, TokenAccount>>,
    pub pool_x: Box<Account<'info, Pool>>,
    #[account(mut, address = pool_x.vault_a)]
    pub x_a: Box<Account<'info, TokenAccount>>,
    #[account(mut, address = pool_x.vault_b)]
    pub x_b: Box<Account<'info, TokenAccount>>,
    pub pool_y: Box<Account<'info, Pool>>,
    #[account(mut, address = pool_y.vault_a)]
    pub y_a: Box<Account<'info, TokenAccount>>,
    #[account(mut, address = pool_y.vault_b)]
    pub y_b: Box<Account<'info, TokenAccount>>,
    #[account(init_if_needed, payer = trader, space = 8 + 8 + 8 + 8 + 32, seeds = [b"stats"], bump)]
    pub stats: Box<Account<'info, Stats>>,
    pub token_program: Program<'info, Token>,
    pub associated_token_program: Program<'info, AssociatedToken>,
    pub system_program: Program<'info, System>,
}

#[account]
pub struct Pool {
    pub authority: Pubkey,
    pub mint_a: Pubkey,
    pub mint_b: Pubkey,
    pub vault_a: Pubkey,
    pub vault_b: Pubkey,
    pub kind: u8,
    pub amp: u64,
    pub fee_bps: u16,
    pub bump: u8,
}

#[account]
pub struct Stats {
    pub total_arbs: u64,
    pub total_profit: u64,
    pub last_profit: u64,
    pub last_trader: Pubkey,
}

#[error_code]
pub enum ArbError {
    #[msg("Invalid pool parameters")]
    BadParams,
    #[msg("Math error or trade too large for the pool")]
    Math,
    #[msg("Output below minimum")]
    Slippage,
    #[msg("Pool reserve cap (1M tokens) exceeded")]
    ReserveCap,
    #[msg("No profitable arbitrage right now")]
    NoArb,
    #[msg("Faucet limit is 10,000 tokens per call")]
    FaucetCap,
}
