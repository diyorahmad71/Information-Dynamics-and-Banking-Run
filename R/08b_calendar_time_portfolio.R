# =====================================================================
# 08b_calendar_time_portfolio.R
# Calendar-time portfolio test (clustering-robust) + naive daily table.
# Sourced in sequence by run_all.R (shared global environment).
#
# FIX SUMMARY (see FIXES.md, items 4 and 5):
#   (a) The old file called itself "clustering-robust" but computed the naive
#       cross-sectional t for each day and then ran a 5-observation t.test()
#       on the five daily means. Neither is robust to the clustering it
#       claimed to address, and the 5-obs test (df = 4) is what produced the
#       p = 0.18 that Section 5.2 now explicitly declines to rely on.
#   (b) The daily significance stars used the normal critical values
#       2.576 / 1.960 / 1.645, which contradict the thesis legend
#       (*** p<0.001, ** p<0.01, * p<0.05, . p<0.10).
#   Both are corrected below. The naive daily table is retained, because
#   Table 4 reports it, but it is now labelled as naive rather than robust.
# =====================================================================

cat("\n=== CALENDAR-TIME PORTFOLIO AND NAIVE DAILY TABLE ===\n")

# ── (1) NAIVE daily cross-sectional statistics, Event 2 ───────
# Retained because Table 4 reports these. They treat banks as independent,
# which Section 5.2 shows they are not, so they size each day's move against
# its own cross-sectional dispersion and are NOT evidence of a sector-wide
# effect. The dependence-adjusted statistic in R/08d is the basis for that.
ct_portfolio <- ew_all %>%
  filter(event_id == 2) %>%
  group_by(Date, day) %>%
  summarise(
    port_AR = mean(AR, na.rm = TRUE),
    n_banks = n(),
    port_se = sd(AR, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(day) %>%
  mutate(
    port_CAR = cumsum(port_AR),
    t_port   = port_AR / port_se,
    # FIX: stars from the p-value on a t distribution with n-1 d.o.f., matching
    # the thesis legend. The old normal cut-offs gave "**" where Table 4 shows
    # "*" (day 0, t = -2.52) and "***" where it shows "*" (day +2, t = +2.59).
    p_port   = 2 * pt(-abs(t_port), df = pmax(n_banks - 1, 1)),
    sig      = case_when(p_port < 0.001 ~ "***",
                         p_port < 0.01  ~ "**",
                         p_port < 0.05  ~ "*",
                         p_port < 0.10  ~ ".",
                         TRUE           ~ "")
  )

cat("\nDaily average abnormal returns, Event 2 (NAIVE -- assumes banks are\n")
cat("independent, which they are not; see R/08d for the corrected statistic):\n")
print(ct_portfolio %>%
  select(day, Date, port_AR, port_se, t_port, p_port, sig, port_CAR, n_banks) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))))

# ── (2) The actual calendar-time portfolio test ───────────────
# This is the specification Chapter 3 describes: on each calendar day inside
# at least one event window, form ONE equally weighted portfolio of the banks
# in that window and regress its return on the market return. The intercept
# estimates the average daily abnormal return, and because every bank sits in
# the same portfolio their co-movement nets out inside the portfolio return
# rather than biasing the standard error. H0: alpha_p = 0.
#
# Note this pools all five event windows, so it is a statement about the
# crisis period as a whole and not about Event 2 alone (Section 5.2 says so).
ct_input <- ew_all %>%
  distinct(ticker, Date, .keep_all = TRUE) %>%   # one row per bank-day
  group_by(Date) %>%
  summarise(port_R  = mean(R_i, na.rm = TRUE),
            Market  = mean(R_m, na.rm = TRUE),
            n_banks = n(), .groups = "drop") %>%
  drop_na()

cat("\nCalendar-time portfolio regression: P_t = alpha_p + beta_p * R_mt + eta_t\n")
cat("Time-series observations (distinct calendar days in >= 1 window):",
    nrow(ct_input), "\n")

ct_reg <- lm(port_R ~ Market, data = ct_input)
ct_hac <- coeftest(ct_reg, vcov = NeweyWest(ct_reg, lag = 2, prewhite = FALSE))
print(ct_hac)

a_p   <- coef(ct_reg)[1]
se_p  <- ct_hac[1, 2]
tcrit <- qt(0.975, df = df.residual(ct_reg))   # FIX: t, not 1.96, at ~15 obs
cat(sprintf("\nDaily abnormal return alpha_p = %.5f (%.3f%%), t = %.3f, p = %.4f\n",
            a_p, 100 * a_p, ct_hac[1, 3], ct_hac[1, 4]))
cat(sprintf("Implied 5-day CAAR = %.2f%%  [95%% CI %.2f%% to %.2f%%]  (t crit %.3f, %d d.f.)\n",
            500 * a_p, 500 * (a_p - tcrit * se_p), 500 * (a_p + tcrit * se_p),
            tcrit, df.residual(ct_reg)))
cat("Caution: beta_p here is estimated on crisis days only, so it is\n")
cat("contaminated by the event itself and should not be read as a normal-times\n")
cat("loading. The test is robust to dependence but low in power.\n")
