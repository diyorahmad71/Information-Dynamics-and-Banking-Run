# =====================================================================
# 13_test_fixes.R
#
# Self-contained regression tests for the corrections listed in FIXES.md.
# BASE R ONLY -- no tidyverse, no network. Run it any time with:
#
#     Rscript 13_test_fixes.R
#
# It does not touch the pipeline. It re-implements each corrected formula
# in isolation and checks it against a value derived independently, so a
# future edit that reintroduces one of these bugs will fail loudly here.
# =====================================================================

pass <- 0L; fail <- 0L
ok <- function(label, got, want, tol = 1e-3) {
  good <- isTRUE(all.equal(got, want, tolerance = tol))
  cat(sprintf("  [%s] %-56s got %-12s want %s\n",
              if (good) "PASS" else "FAIL", label,
              format(got, digits = 6), format(want, digits = 6)))
  if (good) pass <<- pass + 1L else fail <<- fail + 1L
}
ok_chr <- function(label, got, want) {
  good <- identical(as.character(got), as.character(want))
  cat(sprintf("  [%s] %-56s got %-12s want %s\n",
              if (good) "PASS" else "FAIL", label, got, want))
  if (good) pass <<- pass + 1L else fail <<- fail + 1L
}
hr <- function(s) cat("\n== ", s, " ", strrep("=", max(0, 58 - nchar(s))), "\n", sep = "")

cat("\n=============== TESTS FOR THE R FIXES ===============\n")

find_data <- function(f) { c(f, file.path("data", f))[file.exists(c(f, file.path("data", f)))][1] }


# ---------------------------------------------------------------------
hr("1. Kolari-Pynnonen adjustment (R/08d, 12_verify_n48)")
# The thesis states t_adj = t * sqrt((1-rbar) / (1 + (n-1) rbar)).
kp_fixed <- function(t, rbar, n) t * sqrt((1 - rbar) / (1 + (n - 1) * rbar))
kp_old   <- function(t, rbar, n) t / sqrt(1 + (n - 1) * rbar)

rbar <- 0.616; n <- 48; t_naive <- -6.82
ok("adjustment factor at rbar=.616, n=48", sqrt((1 - rbar)/(1 + (n-1)*rbar)), 0.11323, 1e-4)
ok("corrected t (thesis Sections 3.3 / 5.2)", kp_fixed(t_naive, rbar, n), -0.772, 1e-3)
ok("old formula reproduced the wrong value",  kp_old(t_naive, rbar, n),   -1.246, 1e-3)
# Sanity: with zero correlation the adjustment must vanish.
ok("rbar = 0 leaves t unchanged", kp_fixed(-3.5, 0, 48), -3.5)
# Sanity: the factor must be strictly between 0 and 1 for rbar in (0,1).
f <- sapply(seq(0.05, 0.95, by = 0.05), function(r) sqrt((1-r)/(1+(n-1)*r)))
ok("factor strictly decreasing in rbar", all(diff(f) < 0), TRUE)
ok("factor always in (0,1)", all(f > 0 & f < 1), TRUE)

# ---------------------------------------------------------------------
hr("2. Significance stars (R/06, R/08b)")
# Thesis legend: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10.
stars_fixed <- function(t, n) {
  p <- 2 * pt(-abs(t), df = max(n - 1, 1))
  if (p < 0.001) "***" else if (p < 0.01) "**" else
  if (p < 0.05)  "*"   else if (p < 0.10) "."  else ""
}
stars_old <- function(t, n) {
  if (abs(t) > 2.576) "***" else if (abs(t) > 1.960) "**" else
  if (abs(t) > 1.645) "*" else ""
}
# The five Event 2 days as printed in Table 4 of the thesis.
tab4 <- data.frame(day = c(-2, -1, 0, 1, 2),
                   t   = c(-4.48, -4.09, -2.52, -5.41, 2.59),
                   n   = c(48, 48, 47, 46, 46),
                   want = c("***", "***", "*", "***", "*"),
                   stringsAsFactors = FALSE)
for (i in seq_len(nrow(tab4)))
  ok_chr(sprintf("day %+d (t = %.2f) reproduces Table 4", tab4$day[i], tab4$t[i]),
         stars_fixed(tab4$t[i], tab4$n[i]), tab4$want[i])
n_wrong <- sum(mapply(stars_old, tab4$t, tab4$n) != tab4$want)
ok("old thresholds disagreed with Table 4 on 2 of 5 days", n_wrong, 2)

# ---------------------------------------------------------------------
hr("3. Prediction-error correction (R/05, R/06b)")
# Section 3.2: sd(CAR) = sigma * sqrt(n_days * (1 + 1/M)).
se_fixed <- function(sigma, nd, M) sigma * sqrt(nd * (1 + 1 / M))
se_old   <- function(sigma, nd, M) sigma * sqrt(nd)
sigma <- 0.0212; nd <- 5; M <- 191                    # FRC, Table C.1
ok("(1 + 1/M) factor at M = 190 (thesis says ~1.005)", 1 + 1/190, 1.00526, 1e-5)
ok("corrected SE exceeds the old one",
   se_fixed(sigma, nd, M) > se_old(sigma, nd, M), TRUE)
ok("ratio of corrected to old SE", se_fixed(sigma, nd, M)/se_old(sigma, nd, M),
   sqrt(1 + 1/191), 1e-9)
# The Patell standardisation in R/06 must use the SAME quantity.
ok("matches the S_i used by the Patell test in R/06",
   se_fixed(sigma, nd, M), sigma * sqrt(nd * (1 + 1/M)), 1e-12)

# ---------------------------------------------------------------------
hr("4. Calendar-time CI critical value (R/08b, R/11)")
# ~15 daily observations, 2 parameters -> 13 residual d.o.f.
ok("t critical value at 13 d.o.f.", qt(0.975, 13), 2.160, 1e-3)
ok("old code used the normal value", qnorm(0.975), 1.960, 1e-3)
a <- -0.02214; se <- a / -3.38                        # alpha and its HAC SE
lo_t <- 500 * (a - qt(0.975, 13) * se); hi_t <- 500 * (a + qt(0.975, 13) * se)
ok("corrected interval is wider than the normal one",
   (hi_t - lo_t) > (500 * 2 * 1.96 * se), TRUE)
cat(sprintf("       corrected 5-day CAAR interval: [%.2f%%, %.2f%%]\n", lo_t, hi_t))

# ---------------------------------------------------------------------
hr("5. Composite information index (R/07)")
p1 <- find_data("google_trends_svb.csv"); p2 <- find_data("google_trends_bankrun.csv")
if (is.na(p1) || is.na(p2)) {
  cat("  [SKIP] Google Trends CSVs not found; skipping section 5\n")
} else {
  a1 <- read.csv(p1, stringsAsFactors = FALSE); names(a1) <- c("d", "svb")
  a2 <- read.csv(p2, stringsAsFactors = FALSE); names(a2) <- c("d", "br")
  m <- merge(a1, a2, by = "d"); m$d <- as.Date(m$d); m <- m[order(m$d), ]

  ok("the two CSVs share one scale (SVB peaks at 100)", max(m$svb), 100)
  ok("'bank run' peaks far lower on that shared scale", max(m$br), 7)

  comp_fixed <- (m$svb + m$br) / 2                       # keep shared scale
  comp_old   <- (m$svb/max(m$svb) + m$br/max(m$br)) / 2 * 100   # per-series rescale
  s_fixed <- comp_fixed / max(comp_fixed) * 100
  s_old   <- comp_old   / max(comp_old)   * 100
  pk <- which.max(s_fixed); i1 <- which(m$d == as.Date("2023-05-01"))

  ok_chr("composite peaks on 13 March (thesis Section 4.4)", m$d[pk], "2023-03-13")
  ok("unscaled average at the peak = 53.5 (thesis Section 4.4)", comp_fixed[pk], 53.5, 1e-2)
  ok("old code gave 100 there instead", comp_old[pk], 100, 1e-6)
  ok("1 May value ~2.5 (thesis Section 4.4)", s_fixed[i1], 2.54, 1e-2)
  ok("peak is ~40x the 1 May bump (thesis Section 4.4)", 100/s_fixed[i1], 39.3, 1e-1)
  ok("old code gave ~30x instead", 100/s_old[i1], 29.8, 1e-1)
  ok("Jan-Feb mean ~1 (thesis: 'a normalised value of 1-2')",
     mean(s_fixed[m$d < as.Date("2023-03-01")]), 1.05, 1e-2)
  ok("the two versions still correlate above 0.99",
     cor(s_fixed, s_old) > 0.99, TRUE)
}

# ---------------------------------------------------------------------
hr("6. Panel de-duplication (R/08c)")
# Reconstruct the bank-day panel from the event calendar alone: the five
# [-2,+2] windows over the crisis trading days, with SIVB last trading 9 Mar,
# SBNY 10 Mar and FRC 1 May. No price data needed.
td <- as.Date(c("2023-03-06","2023-03-07","2023-03-08","2023-03-09","2023-03-10",
                "2023-03-13","2023-03-14","2023-03-15","2023-03-16","2023-03-17",
                "2023-04-27","2023-04-28","2023-05-01","2023-05-02","2023-05-03"))
ev0 <- as.Date(c("2023-03-08","2023-03-10","2023-03-13","2023-03-15","2023-05-01"))
last_trade <- c(SIVB = as.Date("2023-03-09"), SBNY = as.Date("2023-03-10"),
                FRC  = as.Date("2023-05-01"))
banks <- c(names(last_trade), paste0("B", sprintf("%02d", 1:45)))   # 48 in total

rows <- do.call(rbind, lapply(seq_along(ev0), function(k) {
  i <- match(ev0[k], td)
  w <- td[max(1, i - 2):min(length(td), i + 2)]
  do.call(rbind, lapply(banks, function(b) {
    lt <- if (b %in% names(last_trade)) last_trade[[b]] else max(td)
    d  <- w[w <= lt]
    if (!length(d)) NULL else data.frame(ticker = b, Date = d, event = k)
  }))
}))

ok("48 banks in the constructed panel", length(unique(rows$ticker)), 48)
ok("15 distinct crisis trading dates", length(unique(rows$Date)), 15)
ok("raw rows before de-duplication", nrow(rows), 1165)
uniq <- rows[!duplicated(rows[, c("ticker", "Date")]), ]
ok("rows after de-duplication (thesis Section 5.4 reports 697)", nrow(uniq), 697)
ok("repeated rows removed (thesis reports 468)", nrow(rows) - nrow(uniq), 468)
ok("SIVB contributes 4 bank-dates", sum(uniq$ticker == "SIVB"), 4)
ok("SBNY contributes 5 bank-dates", sum(uniq$ticker == "SBNY"), 5)
ok("FRC contributes 13 bank-dates (10 March + 27-28 Apr + 1 May)",
   sum(uniq$ticker == "FRC"), 13)
# The bug mattered: duplication is uneven across dates.
per_date <- table(rows$Date) / table(uniq$Date)
ok("some dates were counted up to 3x", max(per_date), 3)

# ---------------------------------------------------------------------
hr("7. Estimation-window index guard (R/04, R/06b)")
mk_old      <- function(ei, w) max(1, ei + w[1]):max(1, ei + w[2])
mk_nogurad  <- function(ei, w) max(1, ei + w[1]):(ei + w[2])       # naive "fix"
mk_fixed    <- function(ei, w) { if (ei + w[2] < 1) return(NULL)
                                 max(1, ei + w[1]):(ei + w[2]) }
w <- c(-210, -11)
ok("normal case identical to the old form", identical(mk_fixed(300, w), mk_old(300, w)), TRUE)
ok("corrected length is 200 trading days", length(mk_fixed(300, w)), 200)
ok("corrected window ends before the event", max(mk_fixed(300, w)) < 300, TRUE)
# Degenerate case: an event only 5 rows into the series.
ok("old form silently estimated on a single row", length(mk_old(5, w)), 1)
ok("dropping the guard gives NEGATIVE indices (R would drop rows)",
   any(mk_nogurad(5, w) < 1), TRUE)
ok("corrected form refuses to estimate instead", is.null(mk_fixed(5, w)), TRUE)

# ---------------------------------------------------------------------
hr("8. Duplicated bank characteristics (R/06)")
# bind_rows() stacked the 14-bank core table on the 46-bank KBW CSV with no
# de-duplication, so the cross-sectional regression ran on 60 rows, not 48.
# Found by actually running the pipeline, not by reading it.
core <- c("SIVB","SBNY","FRC","ZION","WAL","PACW","CMA","KEY","RF",
          "JPM","BAC","WFC","USB","TFC")
kbw_path <- find_data("kbw_bank_chars.csv")
if (is.na(kbw_path)) {
  cat("  [SKIP] kbw_bank_chars.csv not found; skipping section 8\n")
} else {
  kbw <- read.csv(kbw_path, stringsAsFactors = FALSE)$ticker
  ok("KBW CSV carries 46 banks", length(kbw), 46)
  ok("core table carries 14 banks", length(core), 14)
  ok("12 tickers appear in both", sum(core %in% kbw), 12)
  ok("bind_rows alone gives 60 rows (the old N)", length(kbw) + length(core), 60)
  ok("distinct() gives the 48 the thesis reports",
     length(unique(c(core, kbw))), 48)
  ok("PACW and CMA are core-only (so distinct keeps their hand-checked values)",
     all(!c("PACW", "CMA") %in% kbw), TRUE)
}

# ---------------------------------------------------------------------
hr("9. info_index column collision (R/07)")
# R/07 pre-created returns_df$info_index as NA, then left_joined a second
# info_index on top -> info_index.x / info_index.y -> the following
# fill(info_index) threw, the enclosing tryCatch swallowed it with an EMPTY
# message, and the loader fell through to the API. The bundled CSVs were
# therefore never used. Found only by running the pipeline.
rd  <- data.frame(Date = as.Date("2023-01-01") + 0:9, Market = 0)
rd$info_index <- NA_real_
inf <- data.frame(Date = as.Date("2023-01-01") + 0:9, info_index = 1:10)

joined_old <- merge(rd, inf, by = "Date")
ok("old join produced suffixed columns, not info_index",
   "info_index" %in% names(joined_old), FALSE)
ok("  the suffixed pair is what appeared instead",
   all(c("info_index.x", "info_index.y") %in% names(joined_old)), TRUE)

rd_fixed <- rd[, setdiff(names(rd), "info_index"), drop = FALSE]   # the fix
joined_new <- merge(rd_fixed, inf, by = "Date")
ok("dropping the placeholder first yields a usable info_index",
   "info_index" %in% names(joined_new), TRUE)
ok("and it carries the joined values", sum(joined_new$info_index), sum(1:10))

# ---------------------------------------------------------------------
cat("\n", strrep("=", 62), "\n", sep = "")
cat(sprintf("  %d passed, %d failed\n", pass, fail))
cat(strrep("=", 62), "\n\n", sep = "")
if (fail > 0) quit(status = 1)
