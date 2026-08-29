# =====================================================================
# 05_abnormal_returns.R
# Abnormal returns, CAR, per-bank event-study results.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 5. ABNORMAL RETURNS ──────────────────────────────────────
event_study <- function(ticker, evt_date) {
  mm <- market_model(ticker, evt_date)
  if (is.null(mm)) return(NULL)
  dates <- returns_df$Date
  ei    <- which.min(abs(as.numeric(dates - evt_date)))
  ew_i  <- max(1, ei + EVT_WIN[1]) : min(nrow(returns_df),
                                          ei + EVT_WIN[2])
  ew <- returns_df[ew_i, ] %>%
    mutate(day = ew_i - ei) %>%   # true offset from event (robust to gaps)
    select(Date, day, R_i = all_of(ticker), R_m = Market) %>%
    drop_na() %>%
    mutate(E_R = mm$alpha + mm$beta * R_m,
           AR  = R_i - E_R)
  n_days <- nrow(ew)
  CAR    <- sum(ew$AR)
  # FIX (prediction-error term). Section 3.2 of the thesis defines the CAR
  # standard deviation WITH the out-of-sample prediction-error correction,
  # sigma_i * sqrt(n_days * (1 + 1/M)), and the Patell standardisation in
  # R/06 already uses exactly that. This line used sigma_i * sqrt(n_days),
  # dropping the (1 + 1/M) factor, so the per-bank t-statistics were slightly
  # larger in absolute value than the stated formula implies. Now consistent
  # with Section 3.2 and with R/06. (The factor is ~1.0026 at M = 190, so no
  # significance verdict changes; it removes an internal inconsistency.)
  se_car <- mm$sigma * sqrt(n_days * (1 + 1 / mm$n))
  t_stat <- CAR / se_car
  list(
    ticker    = ticker,
    evt_date  = evt_date,
    alpha     = mm$alpha,
    beta      = mm$beta,
    R2        = summary(mm$mod)$r.squared,
    sigma     = mm$sigma,
    n_est     = mm$n,  n_days    = n_days,
    CAR       = CAR,
    t_stat    = t_stat,
    p_value   = 2 * pt(-abs(t_stat), df = mm$n - 2),
    sig       = abs(t_stat) > 1.96,
    ew_data   = ew
  )
}
 
message("Running event study...")
results_list <- list()
for (i in seq_len(nrow(EVENTS))) {
  for (t in available) {
    key <- paste0(t, "_e", EVENTS$id[i])
    res <- tryCatch(
      event_study(t, EVENTS$date[i]),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      res$event_id   <- EVENTS$id[i]
      res$event_name <- EVENTS$name[i]
      results_list[[key]] <- res
    }
  }
}
 
results_summary <- map_dfr(results_list, function(r) tibble(
  ticker     = r$ticker,
  event_id   = r$event_id,
  event_name = r$event_name,
  event_date = r$evt_date,
  alpha      = round(r$alpha,  6),
  beta       = round(r$beta,   3),
  R2         = round(r$R2,     3),
  sigma      = round(r$sigma,  5),
  CAR        = round(r$CAR,    4),
  CAR_pct    = paste0(round(r$CAR * 100, 2), "%"),
  t_stat     = round(r$t_stat, 3),
  p_value    = round(r$p_value, 4),
  sig        = r$sig,  n_est = r$n_est,  n_days = r$n_days
))
 
cat("\n=== EVENT STUDY RESULTS ===\n")
print(results_summary %>% select(ticker, event_id, CAR_pct, t_stat, p_value, sig))
