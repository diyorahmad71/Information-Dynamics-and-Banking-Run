# =====================================================================
# fix_analysis.R  —  CORRECTIONS REQUIRED BY THE REVIEW
#
# HOW TO RUN
#   1. Open your thesis-event-study project in RStudio
#   2. source("run_all.R")            # builds ew_all, bank_chars, returns_df
#   3. source("fix_analysis.R")       # this file
#   4. Send me everything printed between the ==== markers
#
# This file fixes three things that CHANGE REPORTED NUMBERS:
#   FIX 1  Panel FE: duplicated bank-dates + wild cluster bootstrap
#   FIX 2  Calendar-time portfolio: proper time-series regression
#   FIX 3  CARs: unequal event-window lengths (common-window check)
# =====================================================================

need <- c("dplyr","tidyr","fixest","sandwich","lmtest")
for (p in need) if (!requireNamespace(p, quietly = TRUE)) {
  message("installing ", p, " ..."); install.packages(p, repos = "https://cloud.r-project.org")
}
invisible(lapply(need, library, character.only = TRUE))

cat("\n\n==================== CORRECTED RESULTS ====================\n")

# ---------------------------------------------------------------------
# FIX 1.  PANEL FE — deduplicate bank-dates, then wild cluster bootstrap
# ---------------------------------------------------------------------
# PROBLEM: the March event windows overlap, so the SAME (ticker, Date)
# entered the panel once per overlapping event. That inflates N from
# ~690 to 1,114 and silently re-weights the regression toward the
# most-overlapped dates. Fix: one row per unique bank-date.

panel_raw <- ew_all %>%
  select(ticker, Date, day, AR, event_id) %>%
  left_join(bank_chars %>% select(ticker, uninsured_dep_pct, assets_bn,
                                  tier1_lev, ltd_ratio, reg_exempt, gsib),
            by = "ticker") %>%
  left_join(returns_df %>% select(Date, info_index), by = "Date") %>%
  drop_na()

cat("\n--- FIX 1: panel de-duplication ---\n")
cat("Rows BEFORE dedup (as reported in thesis):", nrow(panel_raw), "\n")
cat("Duplicated bank-dates:",
    sum(duplicated(panel_raw %>% select(ticker, Date))), "\n")

# Keep ONE observation per bank-date. The abnormal return for a given
# bank on a given day is a single number; which event window it came
# from does not change it.
panel_df <- panel_raw %>%
  distinct(ticker, Date, .keep_all = TRUE) %>%
  mutate(log_assets    = log(assets_bn),
         info_std      = info_index / 100,
         uninsd_x_info = uninsured_dep_pct * info_std,
         assets_x_info = log_assets       * info_std,
         reg_x_info    = reg_exempt       * info_std)

cat("Rows AFTER dedup (correct N):", nrow(panel_df), "\n")
cat("Distinct banks:", n_distinct(panel_df$ticker),
    "| Distinct dates:", n_distinct(panel_df$Date), "\n\n")

m1 <- feols(AR ~ uninsd_x_info                                 | ticker + Date,
            data = panel_df, vcov = "twoway")
m2 <- feols(AR ~ uninsd_x_info + assets_x_info                 | ticker + Date,
            data = panel_df, vcov = "twoway")
m3 <- feols(AR ~ uninsd_x_info + assets_x_info + reg_x_info    | ticker + Date,
            data = panel_df, vcov = "twoway")

cat("=== Panel FE on DEDUPLICATED data (replaces Table 6) ===\n")
print(etable(m1, m2, m3))

# --- Wild cluster bootstrap (imposes the null, resamples by DATE) -----
# With only ~15 date clusters, asymptotic cluster-robust p-values
# over-reject badly (Cameron, Gelbach & Miller 2008). This is the
# correct inference and MUST replace the asymptotic p-value.
wild_boot_p <- function(mod, data, param, cluster_var, B = 999, seed = 20230310) {
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
  list(t_obs = t_obs, p = mean(abs(boot_ts) >= abs(t_obs), na.rm = TRUE),
       n_valid = sum(!is.na(boot_ts)))
}

cat("\n=== Wild cluster bootstrap, clustered by Date, B = 999 (H0: gamma = 0) ===\n")
cat("(this takes a minute or two)\n")
for (mod_name in c("m1", "m2", "m3")) {
  mod <- get(mod_name)
  bt  <- wild_boot_p(mod, panel_df, "uninsd_x_info", "Date", B = 999)
  cat(sprintf("%s : coef = %.5f | t = %.3f | bootstrap p = %.4f  (%d/999 valid)\n",
              mod_name, coef(mod)["uninsd_x_info"], bt$t_obs, bt$p, bt$n_valid))
}

# ---------------------------------------------------------------------
# FIX 2.  CALENDAR-TIME PORTFOLIO — the method Chapter 3 actually describes
# ---------------------------------------------------------------------
# PROBLEM: Chapter 3 describes  P_t = alpha_p + beta_p * R_mt + eta_t
# and inference on the intercept. The old code instead ran a 5-observation
# t.test() on mean daily ARs. Those are different procedures, and the
# 5-obs t-test (df = 4) is what produced the p = 0.18 in the abstract.

cat("\n\n--- FIX 2: calendar-time portfolio (correct specification) ---\n")

# Equal-weighted portfolio of RAW returns for banks in an event window
# FIX: deduplicate bank-dates before averaging. ew_all carries one row per
# (bank, date, EVENT), so the overlapping March windows repeat the same
# bank-day. The mean happens to be unaffected while every bank is duplicated
# the same number of times on a given date, but that is a coincidence of this
# sample, not a property of the estimator, and n_banks was inflated by it.
port <- ew_all %>%
  distinct(ticker, Date, .keep_all = TRUE) %>%
  group_by(Date) %>%
  summarise(port_R = mean(R_i, na.rm = TRUE),
            Market = mean(R_m, na.rm = TRUE),
            n_banks = n(), .groups = "drop") %>%
  drop_na()

cat("Portfolio days (time-series observations):", nrow(port), "\n")
ct_reg <- lm(port_R ~ Market, data = port)
ct_hac <- coeftest(ct_reg, vcov = NeweyWest(ct_reg, lag = 2, prewhite = FALSE))
cat("\nP_t = alpha_p + beta_p * R_mt + eta_t   (Newey-West SE, 2 lags)\n")
print(ct_hac)
a  <- coef(ct_reg)[1]; se <- ct_hac[1, 2]
cat(sprintf("\nDaily abnormal return alpha_p = %.5f (%.3f%%)\n", a, 100 * a))
cat(sprintf("t = %.3f | p = %.4f\n", ct_hac[1, 3], ct_hac[1, 4]))
# FIX: use the t critical value for the regression's own degrees of freedom.
# The interval is built on ~15 daily observations, so the normal value of 1.96
# is too narrow; with 13 d.o.f. the correct multiplier is about 2.16.
tcrit <- qt(0.975, df = df.residual(ct_reg))
cat(sprintf("Implied 5-day CAAR = %.2f%%  [95%% CI %.2f%% to %.2f%%]  (t crit = %.3f on %d d.f.)\n",
            500 * a, 500 * (a - tcrit * se), 500 * (a + tcrit * se),
            tcrit, df.residual(ct_reg)))

# For comparison: the OLD (incorrect) 5-observation t-test
old <- ew_all %>% filter(event_id == 2) %>% group_by(day) %>%
  summarise(port_AR = mean(AR, na.rm = TRUE), .groups = "drop")
cat("\n[for comparison] OLD 5-obs t-test: t =",
    round(t.test(old$port_AR)$statistic, 3),
    "| p =", round(t.test(old$port_AR)$p.value, 4),
    "| df =", round(t.test(old$port_AR)$parameter, 0), "\n")

# ---------------------------------------------------------------------
# FIX 3.  UNEQUAL EVENT WINDOWS — common-window robustness
# ---------------------------------------------------------------------
# PROBLEM: drop_na() means a halted/delisted bank contributes fewer
# trading days, so its CAR sums over a SHORTER window than a survivor's.
# The failed banks drive the tail result, so this must be shown to matter
# or not matter.

cat("\n\n--- FIX 3: event-window length by bank (Event 2) ---\n")
win <- results_summary %>% filter(event_id == 2) %>%
  select(ticker, n_days, CAR) %>% arrange(n_days)
print(as.data.frame(head(win, 12)))
cat("\nDistribution of window lengths:\n"); print(table(win$n_days))

# (a) restrict to banks with the full 5-day window
full_win <- win %>% filter(n_days == max(n_days)) %>% pull(ticker)
cat("\nBanks with a complete window:", length(full_win), "of", nrow(win), "\n")

reg_df2 <- reg_df %>% mutate(full_window = ticker %in% full_win)
m_full <- lm(CAR ~ uninsured_dep_pct + log(assets_bn),
             data = reg_df2 %>% filter(full_window))
cat("\n(a) Parsimonious regression, COMPLETE-WINDOW banks only (N =",
    sum(reg_df2$full_window), "):\n")
print(coeftest(m_full, vcov = vcovHC(m_full, type = "HC3")))

# (b) per-day average AR instead of a raw sum (length-invariant)
cat("\n(b) Same regression using MEAN daily AR (length-invariant):\n")
mean_ar <- ew_all %>% filter(event_id == 2) %>% group_by(ticker) %>%
  summarise(mean_AR = mean(AR, na.rm = TRUE), .groups = "drop")
reg_df3 <- reg_df %>% left_join(mean_ar, by = "ticker")
m_mean  <- lm(mean_AR ~ uninsured_dep_pct + log(assets_bn), data = reg_df3)
print(coeftest(m_mean, vcov = vcovHC(m_mean, type = "HC3")))

cat("\n==================== END — SEND ME EVERYTHING ABOVE ====================\n")
