# =====================================================================
# 08d_robustness_extra.R
# Extra robustness requested in review:
#   (1) leave-one-bank-out sensitivity of the key coefficient
#   (2) bank-resampling bootstrap of the key coefficient
#   (3) cross-sectional-DEPENDENCE-adjusted CAAR test (Kolari-Pynnonen 2010)
# Needs objects from earlier sections: reg_df, results_list.
# Every block is wrapped in tryCatch so it can never stop run_all.R.
# =====================================================================

cat("\n=== EXTRA ROBUSTNESS (Section 8d) ===\n")

## 1. Leave-one-bank-out (parsimonious model) ------------------------
tryCatch({
  loo <- do.call(rbind, lapply(seq_len(nrow(reg_df)), function(i) {
    m  <- lm(CAR ~ uninsured_dep_pct + log(assets_bn), data = reg_df[-i, ])
    ct <- coeftest(m, vcov = vcovHC(m, type = "HC3"))
    data.frame(dropped = reg_df$ticker[i],
               coef    = ct["uninsured_dep_pct", 1],
               p       = ct["uninsured_dep_pct", 4])
  }))
  cat("\n-- Leave-one-bank-out: uninsured-deposit coefficient --\n")
  cat("coef range:", round(min(loo$coef), 5), "to", round(max(loo$coef), 5), "\n")
  cat("still significant at 5% in", sum(loo$p < 0.05), "of", nrow(loo), "drops\n")
  cat("most influential bank:",
      as.character(loo$dropped[which.max(abs(loo$coef - median(loo$coef)))]), "\n")
}, error = function(e) message("  LOO skipped: ", conditionMessage(e)))

## 2. Bank bootstrap of the key coefficient --------------------------
tryCatch({
  set.seed(42); B <- 2000
  bcoef <- replicate(B, {
    idx <- sample(nrow(reg_df), replace = TRUE)
    coef(lm(CAR ~ uninsured_dep_pct + log(assets_bn),
            data = reg_df[idx, ]))["uninsured_dep_pct"]
  })
  cat("\n-- Bootstrap (", B, " bank resamples): uninsured-deposit coef --\n")
  cat("mean:", round(mean(bcoef), 5),
      "| 95% CI [", round(quantile(bcoef, .025), 5), ",",
      round(quantile(bcoef, .975), 5), "]\n")
  cat("share of resamples < 0:", round(mean(bcoef < 0), 3), "\n")
}, error = function(e) message("  Bootstrap skipped: ", conditionMessage(e)))

## 3. Cross-sectional-dependence-adjusted CAAR test ------------------
##    Kolari & Pynnonen (2010): event clustering makes ARs cross-correlated,
##    which inflates the naive cross-sectional t by ~sqrt(1 + (n-1)*rbar).
tryCatch({
  e2 <- Filter(function(r) !is.null(r) && isTRUE(r$event_id == 2), results_list)
  ar_list <- lapply(e2, function(r) setNames(r$ew_data$AR, r$ew_data$day))
  days <- sort(unique(as.integer(unlist(lapply(ar_list, names)))))
  M <- sapply(ar_list, function(v) v[as.character(days)])   # days x banks
  R <- cor(M, use = "pairwise.complete.obs")
  rbar <- mean(R[lower.tri(R)], na.rm = TRUE)
  car_i <- colSums(M, na.rm = TRUE)
  n <- sum(is.finite(car_i))
  t_naive <- mean(car_i) / (sd(car_i) / sqrt(n))
  t_adj   <- t_naive / sqrt(1 + (n - 1) * rbar)
  cat("\n-- Cross-dependence-adjusted CAAR test (Kolari-Pynnonen) --\n")
  cat("avg pairwise AR correlation (rbar):", round(rbar, 3), "\n")
  cat("naive cross-sectional t:", round(t_naive, 2),
      " | dependence-adjusted t:", round(t_adj, 2), "\n")
  cat("(The calendar-time portfolio in Section 8b is the other clustering-robust check.)\n")
}, error = function(e) message("  Cross-dependence test skipped: ", conditionMessage(e)))
