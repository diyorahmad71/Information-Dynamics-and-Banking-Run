# ============================================================
#  Event Study: Information Dynamics & Bank Runs (2023)
#  Master's Thesis — master runner
#
#  Sources the numbered scripts in R/ in order, in a single shared
#  session, reproducing the full analysis end to end. Splitting into
#  sections is organisational only: objects such as results_summary
#  are created early and reused downstream, so the files MUST run in
#  sequence -- which is what this script does.
#
#  BEFORE RUNNING
#  1. Open thesis-event-study.Rproj in RStudio (this sets the working
#     directory to the project root automatically).
#  2. Put the required data files in data/ (see data/README.txt):
#       - SIVB.csv, SBNY.csv, FRC.csv            (delisted banks, Yahoo)
#       - google_trends_svb.csv, google_trends_bankrun.csv
#         (needed for Figure 1; otherwise that figure is skipped)
#  3. First time only: run R/00_packages.R once to install packages.
#  4. Source this file:  source("run_all.R")
#
#  OUTPUT: result CSVs and figures written by R/09 and R/10.
#  NOTE: the working directory must be the project root. Opening the
#  .Rproj does this; if running headless, setwd() to the project root
#  first.
# ============================================================

scripts <- c(
  "R/00_packages.R",
  "R/01_settings_events.R",
  "R/02_data_download.R",
  "R/03_returns_dataframe.R",
  "R/04_market_model.R",
  "R/05_abnormal_returns.R",
  "R/06_aar_caar_characteristics.R",
  "R/06b_ff3_robustness.R",
  "R/07_google_trends.R",
  "R/08_cross_sectional_regression.R",
  "R/08b_calendar_time_portfolio.R",
  "R/08c_panel_fixed_effects.R",
  "R/08d_robustness_extra.R",
  "R/09_plots.R",
  "R/10_save_results.R",
  "R/11_corrections.R"
)

for (s in scripts) {
  message("\n=== Running ", s, " ===")
  source(s, echo = FALSE)
}
message("\nDone. See output/ (and the working directory) for tables and figures.")
