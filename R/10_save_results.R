# =====================================================================
# 10_save_results.R
# Write all result tables and figures to output/.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 10. SAVE RESULTS ─────────────────────────────────────────
write_csv(results_summary, "event_study_results.csv")
write_csv(AAR_table,       "AAR_CAAR_table.csv")
write_csv(reg_df,          "regression_data.csv")
write_csv(bank_chars,      "bank_characteristics.csv")
write_csv(ct_portfolio,    "calendar_time_portfolio.csv")
if (!is.null(ff3_results)) {
  write_csv(ff3_results,    "ff3_event_study_results.csv")
  write_csv(car_comparison, "capm_vs_ff3_comparison.csv")
}
 
cat("\n")
cat("==============================================\n")
cat("  ✓ SCRIPT COMPLETE (VERSION 2)\n")
cat("  Upgrades active:\n")
cat("  [1] KBW expansion — ", length(available), "banks available\n")
cat("  [2] Panel FE regression with Info × UninsuredDep interaction\n")
cat("  [3] Calendar-time portfolio (clustering-robust)\n")
cat("  [4] Google Trends (CSV primary / gtrendsR API fallback)\n")
cat("  [5] Regulatory regime dummy (EGRRCPA 2018)\n")
cat("  Files saved:\n")
cat("  event_study_results.csv\n")
cat("  AAR_CAAR_table.csv\n")
cat("  regression_data.csv\n")
cat("  bank_characteristics.csv (with reg_exempt, gsib dummies)\n")
cat("  calendar_time_portfolio.csv\n")
if (!is.null(ff3_results)) {
  cat("  ff3_event_study_results.csv\n")
  cat("  capm_vs_ff3_comparison.csv\n")
}
if (exists("panel_df") && nrow(panel_df) >= 20)
  cat("  panel_regression_data.csv\n")
cat("  CAAR_plot.png\n")
cat("  CAR_by_bank_E2.png\n")
cat("  uninsured_vs_CAR.png\n")
cat("  calendar_time_portfolio.png\n")
if (!all(is.na(returns_df$info_index)))
  cat("  google_trends_info.png\n")
cat("==============================================\n")
