# =====================================================================
# 12_verify_n48.R
#
# STEP 2 OF 2.  Run this AFTER run_all.R.
#
#   source("00_prepare_pacw_cma.R")   # writes data/PACW.csv, data/CMA.csv
#   source("run_all.R")               # full pipeline, now with 48 banks
#   source("12_verify_n48.R")         # this file
#
# WHAT THIS DOES
#   Prints every number in the thesis that depends on having 48 banks,
#   next to the value currently printed in the thesis, and flags any
#   mismatch. The thesis values are shown ONLY as comparison targets.
#   They are not assumed correct. Where they disagree with the pipeline,
#   the PIPELINE is right and the thesis must be changed.
#
#   The PACW and CMA CARs currently in the thesis (-69.4%, -38.9%) were
#   NOT produced by this pipeline. They were reconstructed algebraically
#   outside it. Block 1 below is the test of whether that reconstruction
#   was correct. Treat a mismatch there as expected and informative, not
#   as a failure of this script.
#
# SEND ME everything printed between the ==== markers.
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(sandwich); library(lmtest)
})
has_quantreg <- requireNamespace("quantreg", quietly = TRUE)
if (!has_quantreg)
  message("note: package 'quantreg' not installed; Block 5 will be skipped. ",
          "install.packages('quantreg') if you want the quantile table.")

hr <- function(s) cat("\n\n========== ", s, " ==========\n")
chk <- function(label, got, thesis, tol) {
  d <- abs(got - thesis)
  cat(sprintf("  %-34s pipeline %9.4f | thesis %9.4f | diff %7.4f  %s\n",
              label, got, thesis, d,
              ifelse(is.na(d), "??", ifelse(d <= tol, "MATCH", "*** MISMATCH ***"))))
}

cat("\n\n==================== VERIFICATION: N = 48 ====================\n")

# ---------------------------------------------------------------------
# BLOCK 0. Did PACW and CMA actually enter the pipeline?
# ---------------------------------------------------------------------
hr("BLOCK 0 -- sample composition")
cat("  banks with usable price data (`available`):", length(available), "\n")
cat("  PACW present:", "PACW" %in% available,
    "| CMA present:", "CMA" %in% available, "\n")
if (!all(c("PACW", "CMA") %in% available))
  stop("PACW and/or CMA did not load. Check that data/PACW.csv and ",
       "data/CMA.csv exist and that 00_prepare_pacw_cma.R ran cleanly.")
cat("  tickers:\n"); print(sort(available))

# ---------------------------------------------------------------------
# BLOCK 1. The critical test: PACW and CMA market model + Event 2 CAR
# ---------------------------------------------------------------------
hr("BLOCK 1 -- PACW / CMA market model and Event 2 CAR  [THE CRITICAL TEST]")
e2 <- results_summary %>% filter(event_id == 2)

cat("\n  Market-model estimates (estimation window [-210,-11]):\n")
print(as.data.frame(
  e2 %>% filter(ticker %in% c("PACW", "CMA", "FRC", "SIVB", "SBNY", "WAL")) %>%
    select(ticker, alpha, beta, R2, sigma, n_est, n_days)))

cat("\n  Event 2 CAR, log-return units (%):\n")
derived <- c(PACW = -69.4, CMA = -38.9)
for (tk in c("PACW", "CMA")) {
  row <- e2 %>% filter(ticker == tk)
  if (nrow(row) == 0) { cat("  ", tk, ": NO RESULT\n"); next }
  chk(paste0(tk, " CAR (%)"), row$CAR * 100, derived[[tk]], 1.0)
  cat(sprintf("       t = %.3f, p = %.4f, est.obs = %d, event days = %d\n",
              row$t_stat, row$p_value, row$n_est, row$n_days))
}
cat("\n  NOTE: the two 'thesis' values above are the reconstructed figures.\n",
    " A difference of more than ~1pp means the reconstruction was wrong and\n",
    " every N=48 number in the thesis must be replaced with this run's output.\n")

cat("\n  Full Event 2 CAR ranking (most to least negative):\n")
print(as.data.frame(e2 %>% arrange(CAR) %>%
        select(ticker, CAR_pct, t_stat, p_value, n_days) %>% head(12)))

# ---------------------------------------------------------------------
# BLOCK 2. Cross-sectional regressions  (thesis Table 5)
# ---------------------------------------------------------------------
hr("BLOCK 2 -- cross-sectional regression, Event 2  (Table 5)")

reg <- results_summary %>% filter(event_id == 2) %>%
  select(ticker, CAR) %>%
  inner_join(bank_chars %>%
               select(ticker, uninsured_dep_pct, assets_bn, tier1_lev, ltd_ratio),
             by = "ticker") %>%
  distinct(ticker, .keep_all = TRUE) %>%
  drop_na()

cat("  N in cross-section:", nrow(reg), " (thesis states 48)\n")
if (nrow(reg) != 48)
  cat("  *** N is not 48. Check bank_chars has rows for PACW and CMA. ***\n")
miss <- setdiff(e2$ticker, reg$ticker)
if (length(miss)) cat("  dropped for missing characteristics:", paste(miss, collapse=", "), "\n")

hc3 <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC3"))
m_full <- lm(CAR ~ uninsured_dep_pct + log(assets_bn) + tier1_lev + ltd_ratio, data = reg)
m_pars <- lm(CAR ~ uninsured_dep_pct + log(assets_bn), data = reg)

cat("\n  FULL model (HC3):\n");         print(hc3(m_full))
cat("  R2 =", round(summary(m_full)$r.squared, 4),
    "| adj R2 =", round(summary(m_full)$adj.r.squared, 4), "\n")
cat("\n  PARSIMONIOUS model (HC3):\n"); print(hc3(m_pars))
cat("  R2 =", round(summary(m_pars)$r.squared, 4),
    "| adj R2 =", round(summary(m_pars)$adj.r.squared, 4), "\n")

cat("\n  --- against the thesis ---\n")
chk("full: uninsured coef",  hc3(m_full)["uninsured_dep_pct","Estimate"], -0.0114, 0.0004)
chk("full: uninsured p",     hc3(m_full)["uninsured_dep_pct","Pr(>|t|)"],  0.015,  0.005)
chk("full: adj R2",          summary(m_full)$adj.r.squared,                0.491,  0.010)
chk("pars: uninsured coef",  hc3(m_pars)["uninsured_dep_pct","Estimate"], -0.0111, 0.0004)
chk("pars: uninsured p",     hc3(m_pars)["uninsured_dep_pct","Pr(>|t|)"],  0.004,  0.004)
chk("pars: adj R2",          summary(m_pars)$adj.r.squared,                0.427,  0.010)

# ---------------------------------------------------------------------
# BLOCK 3. Where the association lives  (thesis Section 5.5)
# ---------------------------------------------------------------------
hr("BLOCK 3 -- subsample trims  (Section 5.5)  [SECOND CRITICAL TEST]")

trim <- function(d, label) {
  m <- lm(CAR ~ uninsured_dep_pct + log(assets_bn), data = d)
  ct <- hc3(m)
  cat(sprintf("  %-32s N=%2d  beta=%+.4f  t=%+.2f  p=%.4f\n",
              label, nrow(d), ct[2,1], ct[2,3], ct[2,4]))
  invisible(c(beta = ct[2,1], p = ct[2,4]))
}
ord <- reg %>% arrange(CAR)
a <- trim(reg,                                        "all banks")
b <- trim(reg %>% filter(!ticker %in% c("SIVB","SBNY","FRC")), "excl. 3 failed")
c3 <- trim(ord %>% slice(-(1:3)),                     "excl. 3 most negative CARs")
c4 <- trim(ord %>% slice(-(1:4)),                     "excl. 4 most negative CARs")
d1 <- trim(ord %>% slice(-(1:round(nrow(ord)*0.10))), "excl. bottom decile")
d2 <- trim(ord %>% slice(-(1:round(nrow(ord)*0.20))), "excl. bottom quintile")

cat("\n  --- against the thesis ---\n")
chk("all: beta",              a[["beta"]],  -0.0111, 0.0004)
chk("excl 3 failed: beta",    b[["beta"]],  -0.0104, 0.0004)
chk("excl 3 failed: p",       b[["p"]],      0.024,  0.010)
chk("excl bottom quintile p", d2[["p"]],     0.903,  0.060)
cat("\n  The 'excl. 3 failed' row is the thesis's headline robustness claim.\n",
    " If its p-value crosses 0.05 in either direction, Section 5.5, the\n",
    " abstract and Section 6.1 all have to be rewritten.\n")

# ---------------------------------------------------------------------
# BLOCK 4. Unequal event windows  (thesis Section 5.5, currently STALE)
# ---------------------------------------------------------------------
hr("BLOCK 4 -- unequal event windows  [thesis number here is known stale]")
win <- e2 %>% select(ticker, n_days) %>% arrange(n_days)
cat("  window-length distribution:\n"); print(table(win$n_days))
full_win <- win %>% filter(n_days == max(n_days)) %>% pull(ticker)
cat("  banks with a complete window:", length(full_win), "of", nrow(win), "\n")

m_cw <- lm(CAR ~ uninsured_dep_pct + log(assets_bn),
           data = reg %>% filter(ticker %in% full_win))
cat("\n  (a) complete-window banks only, N =", sum(reg$ticker %in% full_win), ":\n")
print(hc3(m_cw))

mean_ar <- bind_rows(lapply(results_list, function(r)
             if (r$event_id == 2) tibble(ticker = r$ticker,
                                         mean_AR = mean(r$ew_data$AR)) else NULL))
m_ma <- lm(mean_AR ~ uninsured_dep_pct + log(assets_bn),
           data = reg %>% inner_join(mean_ar, by = "ticker"))
cat("\n  (b) mean daily AR (length-invariant):\n")
print(hc3(m_ma))
cat("\n  Thesis currently prints -0.0132 (p=0.115) for (a) and -0.0043 (p=0.089)\n",
    " for (b). Those came from the old 44-bank run and are EXPECTED to change.\n",
    " Replace them with whatever this block prints.\n")

# ---------------------------------------------------------------------
# BLOCK 5. Quantile regression  (thesis Table 7)
# ---------------------------------------------------------------------
hr("BLOCK 5 -- quantile regression  (Table 7)")
if (has_quantreg) {
  set.seed(20230310)
  taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  qt <- lapply(taus, function(tau) {
    fit <- quantreg::rq(CAR ~ uninsured_dep_pct + log(assets_bn),
                        tau = tau, data = reg)
    s   <- summary(fit, se = "boot", bsmethod = "xy", R = 400)
    co  <- s$coefficients["uninsured_dep_pct", ]
    tibble(tau = tau, beta = co[1], se = co[2], t = co[3], p = co[4])
  })
  print(as.data.frame(bind_rows(qt)), digits = 4)
  cat("\n  thesis Table 7 prints: tau .10 = -0.0203, .25 = -0.0125,",
      "\n  .50 = -0.0124, .75 = -0.0048, .90 = -0.0014\n")
} else {
  cat("  SKIPPED (quantreg not installed)\n")
}

# ---------------------------------------------------------------------
# BLOCK 6. Other robustness the thesis reports at N = 48
# ---------------------------------------------------------------------
hr("BLOCK 6 -- bootstrap, leave-one-out, Huber, non-linearity")

set.seed(20230310)
bs <- replicate(2000, {
  d <- reg[sample(nrow(reg), replace = TRUE), ]
  tryCatch(coef(lm(CAR ~ uninsured_dep_pct + log(assets_bn), d))[2],
           error = function(e) NA_real_)
})
cat(sprintf("  bootstrap (2000): mean %+.4f | 95%% CI [%+.4f, %+.4f]  (thesis: -0.0113, [-0.0179,-0.0049])\n",
            mean(bs, na.rm = TRUE), quantile(bs, .025, na.rm = TRUE),
            quantile(bs, .975, na.rm = TRUE)))

loo <- sapply(seq_len(nrow(reg)), function(i)
  coef(lm(CAR ~ uninsured_dep_pct + log(assets_bn), reg[-i, ]))[2])
cat(sprintf("  leave-one-out: range [%+.4f, %+.4f] | negative in %d of %d | most influential: %s\n",
            min(loo), max(loo), sum(loo < 0), length(loo),
            reg$ticker[which.max(abs(loo - coef(m_pars)[2]))]))
cat("    (thesis: range -0.0096 to -0.0140, negative in all 48, most influential SBNY)\n")

if (requireNamespace("MASS", quietly = TRUE)) {
  hub <- MASS::rlm(CAR ~ uninsured_dep_pct + log(assets_bn), data = reg)
  cat(sprintf("  Huber M-estimator: %+.4f  (thesis: -0.0118)\n", coef(hub)[2]))
}

m_q <- lm(CAR ~ uninsured_dep_pct + I(uninsured_dep_pct^2) + log(assets_bn), data = reg)
cat(sprintf("  quadratic term p = %.3f  (thesis: 0.87)\n", hc3(m_q)[3,4]))
reg$hi <- as.numeric(reg$uninsured_dep_pct > 60)
m_th <- lm(CAR ~ uninsured_dep_pct * hi + log(assets_bn), data = reg)
cat(sprintf("  threshold interaction p = %.3f  (thesis: 0.49) | banks above 60%%: %d\n",
            hc3(m_th)["uninsured_dep_pct:hi", 4], sum(reg$hi)))

# ---------------------------------------------------------------------
# BLOCK 7. Daily-frequency results, now on all 48
# ---------------------------------------------------------------------
hr("BLOCK 7 -- daily-frequency results at N = 48")
cat("  These are currently reported in the thesis on the OLD 46-bank run.\n",
    " Section 4.1 discloses that. After this run they can all be updated.\n\n")

ew2 <- bind_rows(lapply(results_list, function(r)
  if (r$event_id == 2) r$ew_data %>% mutate(ticker = r$ticker) else NULL))
aar <- ew2 %>% group_by(day) %>%
  summarise(AAR = mean(AR), n = n(), .groups = "drop") %>%
  mutate(CAAR = cumsum(AAR))
cat("  AAR / CAAR, Event 2 (thesis Table 4, currently N=46):\n")
print(as.data.frame(aar %>% mutate(AAR = round(100*AAR,2), CAAR = round(100*CAAR,2))))

car2 <- e2$CAR
rbar <- ew2 %>% select(ticker, day, AR) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = AR) %>%
  select(-day) %>% cor(use = "pairwise.complete.obs")
rbar <- mean(rbar[upper.tri(rbar)], na.rm = TRUE)
t_naive <- mean(car2) / (sd(car2) / sqrt(length(car2)))
t_kp    <- t_naive / sqrt(1 + (length(car2) - 1) * rbar)
cat(sprintf("\n  mean pairwise AR correlation = %.3f   (thesis: 0.62)\n", rbar))
cat(sprintf("  naive cross-sectional t = %.2f  ->  Kolari-Pynnonen t = %.2f\n",
            t_naive, t_kp))
cat("    (thesis: -6.49 -> -1.21, on 46 banks)\n")

cat("\n\n=============== END -- SEND ME EVERYTHING ABOVE ===============\n")
