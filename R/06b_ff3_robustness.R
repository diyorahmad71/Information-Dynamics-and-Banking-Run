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
    if (ei + EST_WIN[2] < 1) return(NULL)                  # FIX: see R/04
    est_i <- max(1, ei + EST_WIN[1]) : (ei + EST_WIN[2])
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
    # FIX: same prediction-error correction as R/05 (Section 3.2).
    t_stat <- CAR / (mm$sigma * sqrt(n_days * (1 + 1 / mm$n)))
    tibble(ticker  = ticker,
           CAR_ff3 = round(CAR, 4),
           t_ff3   = round(t_stat, 3),
           p_ff3   = round(2 * pt(-abs(t_stat), df = mm$n - 4), 4),
           sig_ff3 = abs(t_stat) > 1.96)
  }
 
  # FIX (like-for-like benchmark). Table C.5 of the thesis states that "for
  # comparability with the FF3 model, the CAPM CARs in this table are estimated
  # on the excess-return specification (R_i - R_f regressed on R_m - R_f)".
  # The comparison below used to take CAR_capm straight from results_summary,
  # i.e. the RAW-return market model of R/05 -- a different benchmark from the
  # one the note describes, and one that also differs in its risk-free
  # treatment. This adds the excess-return CAPM the note promises, so the
  # CAPM-vs-FF3 columns differ only in the two extra factors.
  event_study_capm_excess <- function(ticker, evt_date) {
    if (!ticker %in% colnames(returns_df)) return(NULL)
    df <- returns_df %>%
      select(Date, R_i = all_of(ticker)) %>%
      inner_join(ff3_df, by = "Date") %>%
      mutate(Excess_i = R_i - RF)
    ei    <- which.min(abs(as.numeric(df$Date - evt_date)))
    if (ei + EST_WIN[2] < 1) return(NULL)                  # FIX: see R/04
    est_i <- max(1, ei + EST_WIN[1]) : (ei + EST_WIN[2])
    est   <- df[est_i, ] %>% drop_na()
    if (nrow(est) < 30) return(NULL)
    mod  <- lm(Excess_i ~ Mkt_RF, data = est)
    sg   <- sd(residuals(mod))
    ew_i <- max(1, ei + EVT_WIN[1]) : min(nrow(df), ei + EVT_WIN[2])
    ew   <- df[ew_i, ] %>% drop_na() %>%
      mutate(E_R = coef(mod)[1] + coef(mod)[2] * Mkt_RF + RF,
             AR  = R_i - E_R)
    CAR <- sum(ew$AR); nd <- nrow(ew)
    tibble(ticker = ticker,
           CAR_capm_x = round(CAR, 4),
           t_capm_x   = round(CAR / (sg * sqrt(nd * (1 + 1 / nrow(est)))), 3))
  }

  capm_x_results <- map_dfr(seq_len(nrow(EVENTS)), function(i) {
    map_dfr(available, function(t) {
      res <- tryCatch(event_study_capm_excess(t, EVENTS$date[i]),
                      error = function(e) NULL)
      if (!is.null(res)) mutate(res, event_id = EVENTS$id[i]) else NULL
    })
  })

  ff3_results <- map_dfr(seq_len(nrow(EVENTS)), function(i) {
    map_dfr(available, function(t) {
      res <- tryCatch(event_study_ff3(t, EVENTS$date[i]),
                      error = function(e) NULL)
      if (!is.null(res)) mutate(res, event_id = EVENTS$id[i]) else NULL
    })
  })
 
  car_comparison <- results_summary %>%
    filter(event_id == 2) %>%
    select(ticker, CAR_raw = CAR, t_raw = t_stat) %>%
    left_join(capm_x_results %>% filter(event_id == 2) %>%
                select(ticker, CAR_capm = CAR_capm_x, t_capm = t_capm_x),
              by = "ticker") %>%
    left_join(ff3_results %>% filter(event_id == 2) %>%
                select(ticker, CAR_ff3, t_ff3, p_ff3),
              by = "ticker") %>%
    mutate(
      CAR_diff_pp  = round((CAR_ff3 - CAR_capm) * 100, 2),
      CAR_raw_pct  = paste0(round(CAR_raw  * 100, 2), "%"),
      CAR_capm_pct = paste0(round(CAR_capm * 100, 2), "%"),
      CAR_ff3_pct  = paste0(round(CAR_ff3  * 100, 2), "%")
    )

  cat("\n=== CAPM vs FF3 COMPARISON (Event 2) ===\n")
  cat("CAR_raw  = raw-return market model of R/05 (this is what Table 3 reports)\n")
  cat("CAR_capm = excess-return CAPM, the like-for-like benchmark Table C.5 describes\n")
  cat("CAR_ff3  = Fama-French three factor\n")
  cat("Diff     = FF3 minus excess-return CAPM, in percentage points\n\n")
  print(car_comparison %>%
    select(ticker, CAR_raw_pct, CAR_capm_pct, t_capm, CAR_ff3_pct, t_ff3, CAR_diff_pp))

  cat("\nCorrelation of raw-return and FF3 CARs (Table 3 vs FF3):",
      round(cor(car_comparison$CAR_raw, car_comparison$CAR_ff3,
                use = "complete.obs"), 4), "\n")
  cat("Mean CAR: raw", round(100 * mean(car_comparison$CAR_raw, na.rm = TRUE), 2),
      "% | FF3", round(100 * mean(car_comparison$CAR_ff3, na.rm = TRUE), 2), "%\n")
  cat("Rank reversals between raw and FF3 orderings:",
      sum(rank(car_comparison$CAR_raw) != rank(car_comparison$CAR_ff3),
          na.rm = TRUE), "of", nrow(car_comparison), "banks\n")
  cat("  (Section 5.5 claims the ordering is 'broadly' preserved -- this line\n")
  cat("   reports exactly how broadly, so the wording can be checked.)\n")
 
  write_csv(ff3_results,    "output/ff3_event_study_results.csv")
  write_csv(capm_x_results, "output/capm_excess_event_study_results.csv")
  write_csv(car_comparison, "output/capm_vs_ff3_comparison.csv")
  message("✓ FF3 robustness complete")
} else {
  ff3_results <- NULL
  message("! Skipping FF3 (download failed)")
}
