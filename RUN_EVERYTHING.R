# =====================================================================
#  RUN_EVERYTHING.R
#  Information Dynamics and Bank Runs — full re-run at N = 48
#
#  ONE FILE. Run it and nothing else.
#
#  WHAT IT DOES, IN ORDER
#    STEP 0  checks you are in the right folder and installs packages
#    STEP 1  converts your PacWest and Comerica downloads into the
#            Yahoo-style CSVs the pipeline expects
#    STEP 2  runs the entire existing pipeline (R/00 ... R/11)
#    STEP 3  verifies every N = 48 number against what the thesis
#            currently claims, and flags each MATCH / MISMATCH
#    STEP 4  writes a full transcript to output/RUN_LOG.txt
#
#  HOW TO RUN
#    1. Open thesis-event-study.Rproj in RStudio  (this sets the
#       working directory; without it nothing will find its files)
#    2. Edit the two file paths in the CONFIG block below
#    3. In the console:   source("RUN_EVERYTHING.R")
#    4. Send me output/RUN_LOG.txt
#
#  EXPECT SOME MISMATCHES. The PacWest and Comerica CARs in the thesis
#  (-69.4%, -38.9%) were reconstructed by hand, outside this pipeline.
#  STEP 3 is the test of whether that reconstruction was right. If it
#  was not, the pipeline is correct and the thesis gets rewritten.
# =====================================================================


# ====================== CONFIG — EDIT THESE ==========================

PACW_RAW <- "~/Downloads/PacWest Stock Price History.csv"
CMA_RAW  <- "~/Downloads/MacroTrends_Data_Download_CMA.csv"

RUN_BOOTSTRAP <- TRUE   # set FALSE for a fast first pass (skips STEP 3 block 6)

# =====================================================================


# ---------------------------------------------------------------------
# STEP 0.  Environment
# ---------------------------------------------------------------------
cat("\n\n################ STEP 0: ENVIRONMENT ################\n")
cat("working directory:", getwd(), "\n")

if (!dir.exists("R") || !file.exists("run_all.R"))
  stop("You are not in the project root. Open thesis-event-study.Rproj in ",
       "RStudio first, or setwd() to the folder that contains run_all.R.")

if (!dir.exists("data"))   dir.create("data")
if (!dir.exists("output")) dir.create("output")

pkgs <- c("quantmod", "dplyr", "tidyr", "readr", "purrr", "tibble",
          "ggplot2", "sandwich", "lmtest", "xts", "zoo", "car",
          "MASS", "quantreg", "fixest")
missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing)) {
  cat("installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble)
  library(sandwich); library(lmtest)
})
still <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(still))
  cat("\n  ! could not install:", paste(still, collapse = ", "),
      "\n    the run will continue and skip whatever needs them\n")

# Start the transcript.
# NOTE: only stdout is redirected, not messages. If messages were sunk too,
# an error would print into the file and the console would look frozen.
# on.exit() does not fire at the top level of a sourced file, so the sink is
# closed by close_log(), which is also registered as the error handler.
LOG <- file.path("output", "RUN_LOG.txt")
con <- file(LOG, open = "wt")

close_log <- function() {
  while (sink.number() > 0) sink()
  try(close(con), silent = TRUE)
  options(error = NULL)
  message("\nTranscript written to ", LOG)
}
options(error = function() { close_log(); })
sink(con, split = TRUE)

cat("run started:", format(Sys.time()), "\n")
cat(R.version.string, "|", Sys.info()[["sysname"]], "\n")


# ---------------------------------------------------------------------
# STEP 1.  Prepare PACW and CMA price files
# ---------------------------------------------------------------------
cat("\n\n################ STEP 1: PACW / CMA PRICE FILES ################\n")

num <- function(x) suppressWarnings(as.numeric(gsub("[,\"$]", "", trimws(x))))

write_yahoo <- function(df, ticker) {
  out <- df %>%
    filter(!is.na(Date), !is.na(Close), Close > 0) %>%
    arrange(Date) %>%
    transmute(Date,
              Open        = ifelse(is.na(Open), Close, Open),
              High        = ifelse(is.na(High), Close, High),
              Low         = ifelse(is.na(Low),  Close, Low),
              Close       = Close,
              `Adj Close` = Close,
              Volume      = ifelse(is.na(Volume), 0, Volume))
  f <- file.path("data", paste0(ticker, ".csv"))
  write_csv(out, f)
  cat(sprintf("  wrote %-16s %4d rows   %s .. %s\n",
              f, nrow(out), min(out$Date), max(out$Date)))
  out
}

# --- PacWest: investing.com layout, MM/DD/YYYY, newest first -----------
if (!file.exists(path.expand(PACW_RAW)))
  stop("PACW file not found at: ", PACW_RAW, "\n  Fix PACW_RAW in the CONFIG block.")
praw <- read.csv(path.expand(PACW_RAW), stringsAsFactors = FALSE,
                 check.names = FALSE, fileEncoding = "UTF-8-BOM")
cat("  PACW raw columns:", paste(names(praw), collapse = " | "), "\n")
if (!all(c("Date", "Price") %in% names(praw)))
  stop("PACW file lacks 'Date'/'Price' columns. Found: ",
       paste(names(praw), collapse = ", "))
pacw_out <- write_yahoo(data.frame(
  Date = as.Date(praw$Date, format = "%m/%d/%Y"),
  Open = num(praw$Open), High = num(praw$High),
  Low  = num(praw$Low),  Close = num(praw$Price),
  Volume = NA_real_), "PACW")

# --- Comerica: MacroTrends, header block then date,open,high,low,close,volume
if (!file.exists(path.expand(CMA_RAW)))
  stop("CMA file not found at: ", CMA_RAW, "\n  Fix CMA_RAW in the CONFIG block.")
cl  <- readLines(path.expand(CMA_RAW), warn = FALSE)
hdr <- grep("^date,open,high,low,close,volume", cl)
if (!length(hdr)) stop("Could not find the data header line in the CMA file.")
craw <- read.csv(text = paste(cl[hdr[1]:length(cl)], collapse = "\n"),
                 stringsAsFactors = FALSE)
cma_out <- write_yahoo(data.frame(
  Date = as.Date(craw$date), Open = num(craw$open), High = num(craw$high),
  Low = num(craw$low), Close = num(craw$close), Volume = num(craw$volume)) %>%
    filter(Date >= as.Date("2022-01-01"), Date <= as.Date("2023-12-31")), "CMA")

# --- coverage checks ---------------------------------------------------
cat("\n  coverage (need first obs on or before 2022-05-05 for a full\n",
    "  [-210,-11] estimation window at Event 2):\n")
for (nm in c("PACW", "CMA")) {
  d  <- if (nm == "PACW") pacw_out else cma_out
  ok <- min(d$Date) <= as.Date("2022-05-05")
  nn <- sum(d$Date >= as.Date("2022-04-01") & d$Date < as.Date("2023-02-23"))
  cat(sprintf("    %-5s first %s  %-16s  %d obs in estimation range\n",
              nm, min(d$Date), ifelse(ok, "OK", "*** TOO LATE ***"), nn))
  if (!ok || nn < 150)
    cat("      ! expect a SHORT DATA warning from R/04_market_model.R\n")
  w <- d %>% filter(Date >= as.Date("2023-03-08"), Date <= as.Date("2023-03-15"))
  cat(sprintf("      event window 8-15 Mar: %d trading days\n", nrow(w)))
}
cat("\n  NOTE: PacWest closes from investing.com are split-adjusted but NOT\n",
    " dividend-adjusted, unlike the Yahoo Adj Close used for every other\n",
    " bank. No ex-div date falls in 8-15 Mar 2023 so the CAR is unaffected,\n",
    " but alpha/beta are estimated over days that do. Disclose in Sec 4.1.\n")


# ---------------------------------------------------------------------
# STEP 2.  Run the pipeline
# ---------------------------------------------------------------------
cat("\n\n################ STEP 2: FULL PIPELINE ################\n")

scripts <- c("R/00_packages.R", "R/01_settings_events.R", "R/02_data_download.R",
             "R/03_returns_dataframe.R", "R/04_market_model.R",
             "R/05_abnormal_returns.R", "R/06_aar_caar_characteristics.R",
             "R/06b_ff3_robustness.R", "R/07_google_trends.R",
             "R/08_cross_sectional_regression.R", "R/08b_calendar_time_portfolio.R",
             "R/08c_panel_fixed_effects.R", "R/08d_robustness_extra.R",
             "R/09_plots.R", "R/10_save_results.R", "R/11_corrections.R",
             "13_test_fixes.R")

failed <- character(0)
for (s in scripts) {
  if (!file.exists(s)) { cat("  SKIP (missing):", s, "\n"); next }
  cat("\n----- ", s, " -----\n")
  ok <- tryCatch({ source(s, echo = FALSE, local = FALSE); TRUE },
                 error = function(e) { cat("  *** ERROR:", conditionMessage(e), "\n"); FALSE })
  if (!ok) failed <- c(failed, s)
}
if (length(failed))
  cat("\n  *** these scripts errored:", paste(failed, collapse = ", "),
      "\n      later results may be missing. Send me the log anyway.\n")


# ---------------------------------------------------------------------
# STEP 3.  Verification
# ---------------------------------------------------------------------
cat("\n\n################ STEP 3: VERIFICATION ################\n")

RESULTS <- list()
chk <- function(label, got, thesis, tol) {
  d <- abs(got - thesis)
  verdict <- if (is.na(d)) "??" else if (d <= tol) "MATCH" else "*** MISMATCH ***"
  cat(sprintf("  %-32s pipeline %9.4f | thesis %9.4f | %s\n",
              label, got, thesis, verdict))
  RESULTS[[label]] <<- verdict
  invisible(got)
}
hr <- function(s) cat("\n---------- ", s, " ----------\n")
hc3 <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC3"))

# --- 3.0 sample --------------------------------------------------------
hr("3.0  sample composition")
cat("  banks loaded:", length(available), "\n")
cat("  PACW in sample:", "PACW" %in% available,
    "| CMA in sample:", "CMA" %in% available, "\n")
if (!all(c("PACW", "CMA") %in% available))
  cat("  *** PACW and/or CMA failed to load. Everything below is still N<48. ***\n")

e2 <- results_summary %>% filter(event_id == 2)

# --- 3.1 THE CRITICAL TEST --------------------------------------------
hr("3.1  PACW / CMA Event 2 CAR   [CRITICAL]")
cat("  market-model estimates:\n")
print(as.data.frame(e2 %>%
  filter(ticker %in% c("PACW","CMA","FRC","SIVB","SBNY","WAL")) %>%
  select(ticker, alpha, beta, R2, sigma, n_est, n_days)))
cat("\n")
for (tk in c("PACW", "CMA")) {
  r <- e2 %>% filter(ticker == tk)
  if (!nrow(r)) { cat("  ", tk, ": NO RESULT\n"); next }
  chk(paste0(tk, " CAR (%)"), r$CAR * 100,
      if (tk == "PACW") -69.4 else -38.9, 1.0)
  cat(sprintf("      t=%.3f p=%.4f  est.obs=%d  event days=%d\n",
              r$t_stat, r$p_value, r$n_est, r$n_days))
}
cat("\n  Event 2 CAR ranking, 12 most negative:\n")
print(as.data.frame(e2 %>% arrange(CAR) %>%
        select(ticker, CAR_pct, t_stat, p_value, n_days) %>% head(12)))

# --- 3.2 cross-section (Table 5) ---------------------------------------
hr("3.2  cross-sectional regression (Table 5)")
reg <- e2 %>% select(ticker, CAR) %>%
  inner_join(bank_chars %>% select(ticker, uninsured_dep_pct, assets_bn,
                                   tier1_lev, ltd_ratio), by = "ticker") %>%
  distinct(ticker, .keep_all = TRUE) %>% drop_na()
cat("  N =", nrow(reg), " (thesis states 48)\n")
drop <- setdiff(e2$ticker, reg$ticker)
if (length(drop)) cat("  dropped for missing characteristics:",
                      paste(drop, collapse = ", "), "\n")

m_full <- lm(CAR ~ uninsured_dep_pct + log(assets_bn) + tier1_lev + ltd_ratio, reg)
m_pars <- lm(CAR ~ uninsured_dep_pct + log(assets_bn), reg)
cat("\n  FULL (HC3):\n");  print(hc3(m_full))
cat("  adj R2 =", round(summary(m_full)$adj.r.squared, 4), "\n")
cat("\n  PARSIMONIOUS (HC3):\n"); print(hc3(m_pars))
cat("  adj R2 =", round(summary(m_pars)$adj.r.squared, 4), "\n\n")
chk("full: uninsured coef", hc3(m_full)["uninsured_dep_pct","Estimate"], -0.0114, 0.0004)
chk("full: uninsured p",    hc3(m_full)["uninsured_dep_pct","Pr(>|t|)"],  0.015,  0.005)
chk("full: adj R2",         summary(m_full)$adj.r.squared,                0.491,  0.010)
chk("pars: uninsured coef", hc3(m_pars)["uninsured_dep_pct","Estimate"], -0.0111, 0.0004)
chk("pars: uninsured p",    hc3(m_pars)["uninsured_dep_pct","Pr(>|t|)"],  0.004,  0.004)
chk("pars: adj R2",         summary(m_pars)$adj.r.squared,                0.427,  0.010)

# --- 3.3 subsample trims (Section 5.5) ---------------------------------
hr("3.3  where the association lives (Sec 5.5)   [CRITICAL]")
trim <- function(d, lab) {
  ct <- hc3(lm(CAR ~ uninsured_dep_pct + log(assets_bn), d))
  cat(sprintf("  %-30s N=%2d  beta=%+.4f  t=%+.2f  p=%.4f\n",
              lab, nrow(d), ct[2,1], ct[2,3], ct[2,4]))
  c(beta = ct[2,1], p = ct[2,4])
}
ord <- reg %>% arrange(CAR)
a  <- trim(reg, "all banks")
b  <- trim(reg %>% filter(!ticker %in% c("SIVB","SBNY","FRC")), "excl. 3 failed")
trim(ord %>% slice(-(1:3)), "excl. 3 worst CARs")
trim(ord %>% slice(-(1:4)), "excl. 4 worst CARs")
trim(ord %>% slice(-(1:round(nrow(ord)*0.10))), "excl. bottom decile")
d2 <- trim(ord %>% slice(-(1:round(nrow(ord)*0.20))), "excl. bottom quintile")
cat("\n")
chk("all: beta",           a[["beta"]], -0.0111, 0.0004)
chk("excl 3 failed: beta", b[["beta"]], -0.0104, 0.0004)
chk("excl 3 failed: p",    b[["p"]],     0.024,  0.010)
chk("bottom quintile: p",  d2[["p"]],    0.903,  0.060)
cat("\n  'excl. 3 failed' is the thesis's headline robustness claim.\n",
    " If its p crosses 0.05, the abstract, Sec 5.5 and Sec 6.1 all change.\n")

# --- 3.4 unequal windows (known stale) ---------------------------------
hr("3.4  unequal event windows   [thesis number known stale]")
print(table(e2$n_days))
fw <- e2 %>% filter(n_days == max(n_days)) %>% pull(ticker)
cat("  complete-window banks:", length(fw), "of", nrow(e2), "\n")
cat("\n  (a) complete-window only:\n")
print(hc3(lm(CAR ~ uninsured_dep_pct + log(assets_bn),
             reg %>% filter(ticker %in% fw))))
mean_ar <- bind_rows(lapply(results_list, function(r)
  if (r$event_id == 2) tibble(ticker = r$ticker, mean_AR = mean(r$ew_data$AR)) else NULL))
cat("\n  (b) mean daily AR (length-invariant):\n")
print(hc3(lm(mean_AR ~ uninsured_dep_pct + log(assets_bn),
             reg %>% inner_join(mean_ar, by = "ticker"))))
cat("\n  thesis prints -0.0132 (p=.115) and -0.0043 (p=.089) from the old\n",
    " 44-bank run. Replace both with whatever is printed above.\n")

# --- 3.5 quantile regression (Table 7) ---------------------------------
hr("3.5  quantile regression (Table 7)")
if (requireNamespace("quantreg", quietly = TRUE)) {
  set.seed(20230310)
  qt <- bind_rows(lapply(c(.10,.25,.50,.75,.90), function(tau) {
    s <- summary(quantreg::rq(CAR ~ uninsured_dep_pct + log(assets_bn),
                              tau = tau, data = reg),
                 se = "boot", bsmethod = "xy", R = 400)
    co <- s$coefficients["uninsured_dep_pct", ]
    tibble(tau = tau, beta = co[1], se = co[2], t = co[3], p = co[4])
  }))
  print(as.data.frame(qt), digits = 4)
  cat("  thesis: .10=-0.0203  .25=-0.0125  .50=-0.0124  .75=-0.0048  .90=-0.0014\n")
} else cat("  SKIPPED (quantreg not installed)\n")

# --- 3.6 other robustness ---------------------------------------------
hr("3.6  bootstrap / leave-one-out / Huber / non-linearity")
if (RUN_BOOTSTRAP) {
  set.seed(20230310)
  bs <- replicate(2000, {
    d <- reg[sample(nrow(reg), replace = TRUE), ]
    tryCatch(coef(lm(CAR ~ uninsured_dep_pct + log(assets_bn), d))[2],
             error = function(e) NA_real_) })
  cat(sprintf("  bootstrap mean %+.4f  95%% CI [%+.4f, %+.4f]   (thesis -0.0113 [-0.0179,-0.0049])\n",
              mean(bs, na.rm=TRUE), quantile(bs,.025,na.rm=TRUE), quantile(bs,.975,na.rm=TRUE)))
} else cat("  bootstrap skipped (RUN_BOOTSTRAP = FALSE)\n")

loo <- sapply(seq_len(nrow(reg)), function(i)
  coef(lm(CAR ~ uninsured_dep_pct + log(assets_bn), reg[-i,]))[2])
cat(sprintf("  leave-one-out range [%+.4f, %+.4f] | negative in %d of %d | most influential %s\n",
            min(loo), max(loo), sum(loo < 0), length(loo),
            reg$ticker[which.max(abs(loo - coef(m_pars)[2]))]))
cat("    (thesis: -0.0096 to -0.0140, negative in all 48, most influential SBNY)\n")

if (requireNamespace("MASS", quietly = TRUE))
  cat(sprintf("  Huber M-estimator %+.4f   (thesis -0.0118)\n",
              coef(MASS::rlm(CAR ~ uninsured_dep_pct + log(assets_bn), data = reg))[2]))

cat(sprintf("  quadratic p = %.3f   (thesis 0.87)\n",
    hc3(lm(CAR ~ uninsured_dep_pct + I(uninsured_dep_pct^2) + log(assets_bn), reg))[3,4]))
reg$hi <- as.numeric(reg$uninsured_dep_pct > 60)
cat(sprintf("  threshold interaction p = %.3f   (thesis 0.49) | banks >60%%: %d\n",
    hc3(lm(CAR ~ uninsured_dep_pct * hi + log(assets_bn), reg))["uninsured_dep_pct:hi",4],
    sum(reg$hi)))

# --- 3.7 daily-frequency at N = 48 -------------------------------------
hr("3.7  daily-frequency results, now on the full sample")
ew2 <- bind_rows(lapply(results_list, function(r)
  if (r$event_id == 2) r$ew_data %>% mutate(ticker = r$ticker) else NULL))
aar <- ew2 %>% group_by(day) %>% summarise(AAR = mean(AR), n = n(), .groups="drop") %>%
  mutate(CAAR = cumsum(AAR))
cat("  AAR / CAAR, Event 2 (thesis Table 4, currently N=46):\n")
print(as.data.frame(aar %>% mutate(AAR = round(100*AAR,2), CAAR = round(100*CAAR,2))))

cm <- ew2 %>% select(ticker, day, AR) %>%
  pivot_wider(names_from = ticker, values_from = AR) %>% select(-day) %>%
  cor(use = "pairwise.complete.obs")
rbar <- mean(cm[upper.tri(cm)], na.rm = TRUE)
tn <- mean(e2$CAR) / (sd(e2$CAR)/sqrt(nrow(e2)))
cat(sprintf("\n  mean pairwise AR correlation %.3f  (thesis 0.62)\n", rbar))
cat(sprintf("  naive t %.2f  ->  Kolari-Pynnonen t %.2f   (thesis -6.49 -> -1.21)\n",
            tn, tn / sqrt(1 + (nrow(e2)-1)*rbar)))

# ---------------------------------------------------------------------
# STEP 4.  Summary
# ---------------------------------------------------------------------
cat("\n\n################ STEP 4: SUMMARY ################\n")
v <- unlist(RESULTS)
cat("  checks run:", length(v),
    "| matched:", sum(v == "MATCH"),
    "| mismatched:", sum(grepl("MISMATCH", v)), "\n\n")
if (any(grepl("MISMATCH", v))) {
  cat("  MISMATCHED:\n")
  for (nm in names(v)[grepl("MISMATCH", v)]) cat("    -", nm, "\n")
  cat("\n  The pipeline is authoritative. Every mismatched value above must\n",
      " be corrected in the thesis, not the other way round.\n")
} else {
  cat("  All checked values reproduce. The reconstructed PacWest and\n",
      "  Comerica CARs were correct and the thesis stands as written.\n")
}
cat("\n  saved objects: results_summary, reg, bank_chars, returns_df\n")
write_csv(reg, file.path("output", "cross_section_N48.csv"))
write_csv(e2 %>% arrange(CAR), file.path("output", "event2_CARs_N48.csv"))
cat("  wrote output/cross_section_N48.csv and output/event2_CARs_N48.csv\n")
cat("\n  SEND ME: output/RUN_LOG.txt\n")
cat("\nrun finished:", format(Sys.time()), "\n")

close_log()
