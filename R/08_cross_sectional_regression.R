# =====================================================================
# 08_cross_sectional_regression.R
# Cross-sectional OLS with HC3 robust SEs + diagnostics.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 8. CROSS-SECTIONAL REGRESSION ───────────────────────────
# Now uses expanded bank sample + regulatory dummy
reg_df <- results_summary %>%
  filter(event_id == 2) %>%
  select(ticker, CAR, sigma) %>%
  left_join(bank_chars, by = "ticker") %>%
  drop_na(uninsured_dep_pct, assets_bn, tier1_lev, ltd_ratio)
 
cat("\n=== CROSS-SECTIONAL REGRESSION (Event 2) ===\n")
cat("N =", nrow(reg_df), "banks\n\n")
 
# Full model
m_full <- lm(CAR ~ uninsured_dep_pct + log(assets_bn) +
               tier1_lev + ltd_ratio, data = reg_df)
cat("--- Full model (HC3 robust SEs) ---\n")
print(coeftest(m_full, vcov = vcovHC(m_full, type = "HC3")))
cat("R-squared:", round(summary(m_full)$r.squared, 3), "\n")
 
# Parsimonious model
m_pars <- lm(CAR ~ uninsured_dep_pct + log(assets_bn),
             data = reg_df)
cat("\n--- Parsimonious model (HC3 robust SEs) ---\n")
print(coeftest(m_pars, vcov = vcovHC(m_pars, type = "HC3")))
cat("R-squared:", round(summary(m_pars)$r.squared, 3), "\n")
 
# UPGRADE 5: Model with regulatory regime dummy
m_reg <- lm(CAR ~ uninsured_dep_pct + log(assets_bn) +
              reg_exempt + gsib, data = reg_df)
cat("\n--- Regulatory model: with EGRRCPA exempt dummy + G-SIB dummy (HC3) ---\n")
cat("(reg_exempt=1: assets < $100B; exempt from 2018 EGRRCPA enhanced standards)\n")
cat("(gsib=1: assets > $700B; JPM, BAC, WFC = Global Systemically Important Banks)\n")
print(coeftest(m_reg, vcov = vcovHC(m_reg, type = "HC3")))
cat("R-squared:", round(summary(m_reg)$r.squared, 3), "\n")
 
# Robustness: exclude failed banks
reg_nofailed <- reg_df %>%
  filter(!ticker %in% c("SIVB", "SBNY", "FRC"))
m_rob <- lm(CAR ~ uninsured_dep_pct + log(assets_bn),
            data = reg_nofailed)
cat("\n--- Robustness: exclude failed banks (N =", nrow(reg_nofailed), ") ---\n")
print(coeftest(m_rob, vcov = vcovHC(m_rob, type = "HC3")))
