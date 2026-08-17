# =====================================================================
# 06b_ff3_robustness.R
# Fama-French 3-factor robustness check.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 6b. FAMA-FRENCH 3 ROBUSTNESS ─────────────────────────────
if (!is.null(ff3_df)) {
  message("\nRunning Fama-French 3-factor robustness check...")
 
  market_model_ff3 <- function(ticker, evt_date) {
    if (!ticker %in% colnames(returns_df)) return(NULL)
    df <- returns_df %>%
      select(Date, R_i = all_of(ticker)) %>%
      inner_join(ff3_df, by = "Date") %>%
      mutate(Excess_i = R_i - RF)
    dates <- df$Date
    ei    <- which.min(abs(as.numeric(dates - evt_date)))
    est_i <- max(1, ei + EST_WIN[1]) : max(1, ei + EST_WIN[2])
    est   <- df[est_i, ] %>% drop_na()
    if (nrow(est) < 30) return(NULL)
    mod <- lm(Excess_i ~ Mkt_RF + SMB + HML, data = est)
    list(alpha  = coef(mod)[1], b_mkt  = coef(mod)[2],
         b_smb  = coef(mod)[3], b_hml  = coef(mod)[4],
         sigma  = sd(residuals(mod)), n = nrow(est), df_mrg = df)
  }
 
  event_study_ff3 <- function(ticker, evt_date) {
    mm <- market_model_ff3(ticker, evt_date)
    if (is.null(mm)) return(NULL)
    dates  <- mm$df_mrg$Date
    ei     <- which.min(abs(as.numeric(dates - evt_date)))
    ew_i   <- max(1, ei + EVT_WIN[1]) :
              min(nrow(mm$df_mrg), ei + EVT_WIN[2])
    ew     <- mm$df_mrg[ew_i, ] %>% drop_na() %>%
      mutate(E_R = mm$alpha + mm$b_mkt*Mkt_RF +
                   mm$b_smb*SMB + mm$b_hml*HML + RF,
             AR  = R_i - E_R)
    n_days <- nrow(ew)
    CAR    <- sum(ew$AR)
    t_stat <- CAR / (mm$sigma * sqrt(n_days))
    tibble(ticker  = ticker,
           CAR_ff3 = round(CAR, 4),
           t_ff3   = round(t_stat, 3),
           p_ff3   = round(2 * pt(-abs(t_stat), df = mm$n - 4), 4),
           sig_ff3 = abs(t_stat) > 1.96)
  }
 
  ff3_results <- map_dfr(seq_len(nrow(EVENTS)), function(i) {
    map_dfr(available, function(t) {
      res <- tryCatch(event_study_ff3(t, EVENTS$date[i]),
                      error = function(e) NULL)
      if (!is.null(res)) mutate(res, event_id = EVENTS$id[i]) else NULL
    })
  })
 
  car_comparison <- results_summary %>%
    filter(event_id == 2) %>%
    select(ticker, CAR_capm = CAR, t_capm = t_stat, p_capm = p_value) %>%
    left_join(ff3_results %>% filter(event_id == 2) %>%
                select(ticker, CAR_ff3, t_ff3, p_ff3),
              by = "ticker") %>%
    mutate(
      CAR_diff_pp  = round((CAR_ff3 - CAR_capm) * 100, 2),
      CAR_capm_pct = paste0(round(CAR_capm * 100, 2), "%"),
      CAR_ff3_pct  = paste0(round(CAR_ff3  * 100, 2), "%")
    )
 
  cat("\n=== CAPM vs FF3 COMPARISON (Event 2) ===\n")
  print(car_comparison %>%
    select(ticker, CAR_capm_pct, t_capm, CAR_ff3_pct, t_ff3, CAR_diff_pp))
 
  write_csv(ff3_results,    "ff3_event_study_results.csv")
  write_csv(car_comparison, "capm_vs_ff3_comparison.csv")
  message("✓ FF3 robustness complete")
} else {
  ff3_results <- NULL
  message("! Skipping FF3 (download failed)")
}
