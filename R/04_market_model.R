# =====================================================================
# 04_market_model.R
# Market-model (OLS) estimation over the estimation window.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 4. MARKET MODEL ──────────────────────────────────────────
market_model <- function(ticker, evt_date) {
  if (!ticker %in% colnames(returns_df)) return(NULL)
  dates  <- returns_df$Date
  ei     <- which.min(abs(as.numeric(dates - evt_date)))
  est_i  <- max(1, ei + EST_WIN[1]) : max(1, ei + EST_WIN[2])
  est    <- returns_df[est_i, ] %>%
    select(Date, R_i = all_of(ticker), R_m = Market) %>% drop_na()
  if (nrow(est) < 30) return(NULL)
  target <- abs(EST_WIN[1] - EST_WIN[2]) + 1          # intended window (~200 days)
  if (nrow(est) < 0.75 * target)
    message("  ! SHORT DATA: ", ticker, " has only ", nrow(est),
            " estimation obs (target ~", target,
            ") -- its CSV likely starts too late; extend it back to ~April 2022.")
  mod <- lm(R_i ~ R_m, data = est)
  list(alpha = coef(mod)[1], beta = coef(mod)[2],
       sigma = sd(residuals(mod)), n = nrow(est), mod = mod)
}
