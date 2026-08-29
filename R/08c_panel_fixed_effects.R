# =====================================================================
# 08c_panel_fixed_effects.R
# Panel fixed-effects regression.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── UPGRADE 2: PANEL FIXED-EFFECTS REGRESSION ────────────────
# Tests the core thesis claim: do information dynamics (Google Trends)
# amplify balance-sheet vulnerabilities?
#
# Specification:
#   AR_it = γ1(UninsuredDep_i × InfoIndex_t) + γ2(log(Assets)_i × InfoIndex_t)
#           + μi (bank FE) + δt (date FE) + u_it
#
# μi absorbs time-invariant bank heterogeneity; δt absorbs market-wide shocks.
# The interaction coefficient γ1 is the key parameter: a negative γ1 means
# that on days with high Google Trends (high public attention), banks with
# more uninsured deposits suffer larger abnormal losses.
#
# Package: fixest (fast fixed effects, two-way clustered SE support)
 
if (!is.na(returns_df$info_index[1]) &&
    !all(is.na(returns_df$info_index))) {
 
  cat("\n=== UPGRADE 2: PANEL FIXED-EFFECTS REGRESSION ===\n")
  cat("AR_it = γ(UninsuredDep_i × InfoIndex_t) + γ(logAssets_i × InfoIndex_t)",
      "+ bank FE + date FE\n\n")
 
  # Build panel data: daily abnormal returns × bank characteristics × info index
  # Use ALL events to maximize panel dimension
  panel_df <- ew_all %>%
    select(ticker, Date, day, AR, event_id) %>%
    left_join(
      bank_chars %>% select(ticker, uninsured_dep_pct, assets_bn,
                            tier1_lev, ltd_ratio, reg_exempt, gsib),
      by = "ticker"
    ) %>%
    left_join(
      returns_df %>% select(Date, info_index),
      by = "Date"
    ) %>%
    drop_na() %>%
    # FIX (duplicated bank-dates). ew_all holds one row per (bank, date, EVENT).
    # The four March event windows overlap on the calendar, so the SAME bank-day
    # entered this panel once per containing window -- inflating N from ~697 to
    # ~1,165 and re-weighting the estimator toward the most-overlapped dates. A
    # bank's abnormal return on a given day is a single number; which window it
    # came from does not change it. R/11 used to repair this after the fact;
    # doing it here means the main pipeline reports the thesis's N = 697 directly.
    distinct(ticker, Date, .keep_all = TRUE) %>%
    mutate(
      log_assets    = log(assets_bn),
      # Standardise info_index to [0,1] for coefficient interpretability
      info_std      = info_index / 100,
      uninsd_x_info = uninsured_dep_pct * info_std,
      assets_x_info = log_assets * info_std,
      reg_x_info    = reg_exempt * info_std
    )
 
  if (nrow(panel_df) >= 20 && length(unique(panel_df$ticker)) >= 5) {
 
    # Model 1: Basic interaction (uninsured deposits × information)
    m_panel_1 <- feols(
      AR ~ uninsd_x_info | ticker + Date,
      data   = panel_df,
      vcov   = "twoway"  # two-way clustered SE: bank + date
    )
 
    # Model 2: Full interactions
    m_panel_2 <- feols(
      AR ~ uninsd_x_info + assets_x_info | ticker + Date,
      data   = panel_df,
      vcov   = "twoway"
    )
 
    # Model 3: Add regulatory regime interaction
    m_panel_3 <- feols(
      AR ~ uninsd_x_info + assets_x_info + reg_x_info | ticker + Date,
      data   = panel_df,
      vcov   = "twoway"
    )
 
    cat("Model 1: AR ~ UninsuredDep × InfoIndex | bank FE + date FE\n")
    print(summary(m_panel_1))
    cat("\nModel 2: Adding log(Assets) × InfoIndex\n")
    print(summary(m_panel_2))
    cat("\nModel 3: Adding reg_exempt × InfoIndex\n")
    print(summary(m_panel_3))
 
    # FIX (inference). With ~15 date clusters, asymptotic cluster-robust
    # p-values over-reject badly (Cameron, Gelbach & Miller 2008), so the
    # printed "twoway" p-values above must NOT be quoted. The wild cluster
    # bootstrap below imposes the null and resamples by date; it is the basis
    # for the p-values reported in Table 6. R/11 used to add this after the
    # fact -- running it here means the main pipeline reports the right number.
    wild_boot_p <- function(mod, data, param, cluster_var,
                            B = 999, seed = 20230310) {
      set.seed(seed)
      t_obs    <- summary(mod)$coeftable[param, "t value"]
      clusters <- unique(data[[cluster_var]])
      resid_v  <- resid(mod); fitted_v <- fitted(mod)
      yvar     <- as.character(formula(mod))[2]
      boot_ts  <- numeric(B)
      for (b in seq_len(B)) {
        w <- sample(c(-1, 1), length(clusters), replace = TRUE)
        names(w) <- as.character(clusters)
        data_b <- data
        data_b[[yvar]] <- fitted_v + resid_v * w[as.character(data[[cluster_var]])]
        mod_b <- tryCatch(feols(formula(mod), data = data_b, vcov = "twoway"),
                          error = function(e) NULL)
        boot_ts[b] <- if (!is.null(mod_b)) summary(mod_b)$coeftable[param, "t value"] else NA
      }
      list(t_obs = t_obs, p = mean(abs(boot_ts) >= abs(t_obs), na.rm = TRUE))
    }

    cat("\n--- Wild cluster bootstrap by date, B = 999 (H0: gamma = 0) ---\n")
    cat("    (this is the inference Table 6 reports; the asymptotic\n")
    cat("     cluster-robust p-values above are unreliable at 15 clusters)\n")
    cat("N =", nrow(panel_df), "bank-days |",
        n_distinct(panel_df$ticker), "banks |",
        n_distinct(panel_df$Date), "dates\n")
    for (mn in c("m_panel_1", "m_panel_2", "m_panel_3")) {
      bt <- wild_boot_p(get(mn), panel_df, "uninsd_x_info", "Date")
      cat(sprintf("  %s : coef = %+.5f | t = %+.3f | bootstrap p = %.4f\n",
                  mn, coef(get(mn))["uninsd_x_info"], bt$t_obs, bt$p))
    }

    cat("\n--- Interpretation ---\n")
    cat("γ1 (UninsuredDep × Info): A negative coefficient means banks with\n")
    cat("  higher uninsured deposits suffer larger abnormal losses on high-\n")
    cat("  attention days — the information dynamics channel.\n")
    cat("γ2 (logAssets × Info): Positive = size buffers against panic-driven losses.\n")
    cat("γ3 (RegExempt × Info): Negative = regulatory loophole exposed under scrutiny.\n")
 
    write_csv(panel_df, "panel_regression_data.csv")
    message("✓ Panel data saved: panel_regression_data.csv")
 
  } else {
    cat("Insufficient panel data (N =", nrow(panel_df),
        "obs,", length(unique(panel_df$ticker)), "banks)\n")
  }
} else {
  cat("\n! Panel FE skipped: Google Trends info_index unavailable.\n")
}
