# =====================================================================
# 08b_calendar_time_portfolio.R
# Calendar-time portfolio test (clustering-robust).
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── UPGRADE 3: CALENDAR-TIME PORTFOLIO TEST ──────────────────
# Addresses event clustering: all banks hit simultaneously.
# Instead of testing each bank's CAR individually, form a calendar-time
# portfolio of abnormal returns each day and test if the portfolio return
# is significantly different from zero. This is clustering-robust.
# Reference: MacKinlay (1997, Journal of Economic Literature, pp. 20-21)
 
cat("\n=== UPGRADE 3: CALENDAR-TIME PORTFOLIO (Event 2) ===\n")
cat("(clustering-robust test: portfolio of daily ARs)\n\n")
 
ct_portfolio <- ew_all %>%
  filter(event_id == 2) %>%
  group_by(Date, day) %>%
  summarise(
    port_AR = mean(AR, na.rm = TRUE),
    n_banks  = n(),
    port_se  = sd(AR, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  arrange(day) %>%
  mutate(
    port_CAR = cumsum(port_AR),
    t_port   = port_AR / port_se,
    sig      = case_when(abs(t_port) > 2.576 ~ "***",
                         abs(t_port) > 1.960 ~ "**",
                         abs(t_port) > 1.645 ~ "*",
                         TRUE                ~ "")
  )
 
cat("Daily portfolio abnormal returns (Event 2):\n")
print(ct_portfolio %>%
  select(day, Date, port_AR, port_se, t_port, sig, port_CAR, n_banks) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))))
 
# Overall portfolio test: is mean daily AR ≠ 0 over the event window?
t_overall <- t.test(ct_portfolio$port_AR, mu = 0)
cat("\nCalendar-time portfolio t-test (H0: mean daily port AR = 0):\n")
cat("  Mean AR:", round(mean(ct_portfolio$port_AR), 4),
    "| t =", round(t_overall$statistic, 3),
    "| p =", round(t_overall$p.value, 4), "\n")
