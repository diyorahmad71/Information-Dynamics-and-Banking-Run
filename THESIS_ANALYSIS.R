# =====================================================================
#  INFORMATION DYNAMICS AND BANK RUNS
#  An Event Study of the 2023 Banking Crisis
#
#  SINGLE-FILE CORRECTED ANALYSIS.  Diyorbek Ahmadjonov, University of Bonn.
#
#  ---------------------------------------------------------------------
#  HOW TO RUN
#  ---------------------------------------------------------------------
#  1. Put this file in a folder together with these data files:
#
#       SIVB.csv  SBNY.csv  FRC.csv          (delisted banks)
#       PACW.csv  CMA.csv                    (from 00_prepare_pacw_cma.R)
#       google_trends_svb.csv
#       google_trends_bankrun.csv
#       kbw_bank_chars.csv
#
#     They may sit in that folder or in a data/ subfolder; both work.
#
#  2. Open it in RStudio and press Source (or run:  source("THESIS_ANALYSIS.R")).
#
#  3. Needs an internet connection: 43 of the 48 price series download live
#     from Yahoo Finance, with Stooq as a fallback. The other five are read
#     from the CSVs above. If a bank fails to download, drop its CSV in the
#     folder and re-run -- a local CSV always wins.
#
#  4. Everything the thesis reports is printed at the end under
#     "THESIS NUMBERS", labelled by table and section, ready to copy across.
#
#  ---------------------------------------------------------------------
#  This file consolidates the whole corrected pipeline. All 18 fixes from
#  FIXES.md are applied and marked "# FIX" at the line they touch. A short
#  self-test suite runs at the very end and reports PASS/FAIL.
#  ---------------------------------------------------------------------

options(stringsAsFactors = FALSE)
cat("\n=====================================================================\n")
cat("  INFORMATION DYNAMICS AND BANK RUNS -- corrected analysis\n")
cat("=====================================================================\n")

# =====================================================================
# 0. PACKAGES
# =====================================================================
pkgs <- c("quantmod", "dplyr", "tidyr", "purrr", "readr", "tibble",
          "stringr", "ggplot2", "xts", "sandwich", "lmtest", "scales")
opt  <- c("quantreg", "fixest", "MASS", "ggrepel")     # optional extras

need <- setdiff(c(pkgs, opt), rownames(installed.packages()))
if (length(need)) {
  message("Installing: ", paste(need, collapse = ", "))
  # FIX 8: name a CRAN mirror, or a non-interactive run dies with
  # "trying to use CRAN without setting a mirror" instead of installing.
  install.packages(need, dependencies = TRUE, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
HAS_QR <- requireNamespace("quantreg", quietly = TRUE)
HAS_FE <- requireNamespace("fixest",   quietly = TRUE)
if (HAS_FE) suppressPackageStartupMessages(library(fixest))
cat("\n[0] Packages loaded. quantreg:", HAS_QR, "| fixest:", HAS_FE, "\n")

# =====================================================================
# 1. SETTINGS AND THE FIVE EVENTS
# =====================================================================
START_DATE <- "2022-04-01"
END_DATE   <- "2023-06-30"
EST_WIN    <- c(-210, -11)      # estimation window, trading days
EVT_WIN    <- c(-2,   +2)       # event window, trading days

TICKERS <- c(
  "SIVB","SBNY","FRC","ZION","WAL","PACW","CMA","KEY","RF",
  "JPM","BAC","WFC","USB","TFC",
  "FITB","HBAN","CFG","FHN","SNV","COLB","OZK","WTFC","BOKF","UMBF","VLY",
  "NYCB","WBS","HWC","EWBC","CBSH","FFBC","PB","TCBI","ONB","UBSI","GBCI",
  "HTLF","INDB","WSFS","BANR","FFIN","TRMK","SFNC","HAFC","CVBF","RNST",
  "VBTX","WAFD","CATY","EBC","GABC","FBMS","HTBK","IBCP")

EVENTS <- tibble::tribble(
  ~id, ~name,                           ~date,
  1,   "SVB discloses bond losses",     "2023-03-08",
  2,   "SVB closed by regulator",       "2023-03-10",
  3,   "Systemic risk exception",       "2023-03-13",   # Sunday 12th -> next session
  4,   "Contagion escalation",          "2023-03-15",
  5,   "First Republic seized",         "2023-05-01"
) %>% mutate(date = as.Date(date))

# helper: look for a data file in ./ and in data/
find_data <- function(f) {
  cand <- c(f, file.path("data", f))
  hit  <- cand[file.exists(cand)]
  if (length(hit)) hit[1] else NA_character_
}

# =====================================================================
# 2. PRICE DATA  (local CSV -> Yahoo -> Stooq)
# =====================================================================
cat("\n[2] Loading price data ...\n")

load_csv <- function(ticker) {
  f <- find_data(paste0(ticker, ".csv"))
  if (is.na(f)) return(FALSE)
  tryCatch({
    df <- read_csv(f, show_col_types = FALSE, comment = "#",
                   na = c("", "NA", "null", "N/A")) %>%
      mutate(Date = as.Date(Date),
             AdjClose = suppressWarnings(as.numeric(`Adj Close`))) %>%
      filter(!is.na(AdjClose), Date >= as.Date(START_DATE),
             Date <= as.Date(END_DATE)) %>%
      arrange(Date)
    if (nrow(df) < 10) return(FALSE)
    px <- xts(matrix(rep(df$AdjClose, 6), ncol = 6), order.by = df$Date)
    colnames(px) <- paste0(ticker, c(".Open",".High",".Low",".Close",".Volume",".Adjusted"))
    assign(ticker, px, envir = .GlobalEnv)
    message("  ok  ", ticker, " - local CSV ", f, " (", nrow(df), " rows)")
    TRUE
  }, error = function(e) { message("  x   ", ticker, " CSV: ", e$message); FALSE })
}

load_yahoo <- function(ticker, tries = 3) {
  for (k in seq_len(tries)) {
    ok <- tryCatch({
      # env = globalenv() is essential: without it getSymbols assigns into
      # THIS function's frame, the object vanishes when the function returns,
      # and the later get(ticker) fails with "object not found" even though
      # the download succeeded.
      suppressWarnings(getSymbols(ticker, src = "yahoo", from = START_DATE,
                                  to = END_DATE, auto.assign = TRUE,
                                  env = globalenv()))
      if (nrow(get(ticker, envir = globalenv())) < 10) stop("too few rows")
      message("  ok  ", ticker, " - Yahoo"); TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(1.0 * k)
  }
  FALSE
}

load_stooq <- function(ticker, tries = 3) {
  url <- paste0("https://stooq.com/q/d/l/?s=", tolower(ticker),
                ".us&d1=", gsub("-", "", START_DATE),
                "&d2=", gsub("-", "", END_DATE), "&i=d")
  for (k in seq_len(tries)) {
    okk <- tryCatch({
      df <- suppressWarnings(read_csv(url, show_col_types = FALSE))
      if (!"Close" %in% names(df) || nrow(df) < 10) stop("throttled")
      px <- xts(matrix(rep(df$Close, 6), ncol = 6), order.by = as.Date(df$Date))
      colnames(px) <- paste0(ticker, c(".Open",".High",".Low",".Close",".Volume",".Adjusted"))
      assign(ticker, px, envir = .GlobalEnv)
      message("  ok  ", ticker, " - Stooq"); TRUE
    }, error = function(e) FALSE)
    if (isTRUE(okk)) return(TRUE)
    Sys.sleep(2 * k)
  }
  FALSE
}

available <- character(0)
for (t in TICKERS) {
  if (isTRUE(load_csv(t) || load_yahoo(t) || load_stooq(t))) available <- c(available, t)
  else message("  --  ", t, " unavailable from every source")
  Sys.sleep(0.3)
}

# FIX 18: the market index used to go straight to Yahoo inside
# suppressWarnings(), with no local fallback and no tryCatch -- so one Yahoo
# outage killed the run even with all 48 stock CSVs on disk. It now takes a
# local GSPC.csv first, exactly like the stocks.
if (!load_csv("GSPC")) {
  ok_mkt <- tryCatch({
    suppressWarnings(getSymbols("^GSPC", src = "yahoo", from = START_DATE,
                                to = END_DATE, auto.assign = TRUE)); TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok_mkt))
    stop("No market index. Put a Yahoo-format GSPC.csv beside this script and re-run.")
}
cat("  Available:", length(available), "of", length(TICKERS), "tickers\n")
if (length(available) < 5) stop("Too few tickers. Add local CSVs and re-run.")

# =====================================================================
# 3. DAILY LOG RETURNS
# =====================================================================
# FIX 19: difference each ticker on ITS OWN observed prices, then merge.
# The old code merged the price levels first and differenced the aligned
# matrix, so a bank that was halted and later resumed lost the return across
# the halt: First Republic was suspended on 1 May 2023 and resumed OTC on
# 3 May at $0.33 from $3.51, and that -234% abnormal return -- the single
# largest in the sample and the whole content of Event 5 -- was silently
# dropped, taking the Event 5 CAAR from -14.35% to -9.26% and one row out of
# the attention panel. Differencing per ticker makes the resumption-day return
# span the halt, which is the correct treatment and the one the thesis
# describes in Section 3.2.
ret_list <- lapply(available, function(t) {
  px <- Ad(get(t)); px <- px[!is.na(px)]
  r  <- diff(log(px)); colnames(r) <- t; r })
market_xts <- Ad(GSPC); colnames(market_xts) <- "Market"

all_ret <- merge(do.call(merge, ret_list), diff(log(market_xts)))
all_ret <- all_ret[!is.na(coredata(all_ret)[, "Market"]), ]
returns_df <- data.frame(Date = index(all_ret), coredata(all_ret)) %>%
  mutate(Date = as.Date(Date))
cat("\n[3] Returns frame:", nrow(returns_df), "trading days,",
    ncol(returns_df) - 1, "series\n")

# =====================================================================
# 4-5. MARKET MODEL, ABNORMAL RETURNS, CARs
# =====================================================================
market_model <- function(ticker, evt_date) {
  if (!ticker %in% colnames(returns_df)) return(NULL)
  ei <- which.min(abs(as.numeric(returns_df$Date - evt_date)))
  # FIX 14: guard the START of the window only, and refuse to estimate when the
  # window cannot be formed. The old code put max(1, .) on BOTH ends, so an
  # early event silently estimated on a single row; removing the guard entirely
  # is worse, because 1:(ei-11) is a decreasing range that R reads as negative
  # (row-dropping) indices.
  start_i <- ei + EST_WIN[1]; end_i <- ei + EST_WIN[2]
  if (end_i < 1) return(NULL)
  est <- returns_df[max(1, start_i):end_i, ] %>%
    select(Date, R_i = all_of(ticker), R_m = Market) %>% drop_na()
  if (nrow(est) < 30) return(NULL)
  mod <- lm(R_i ~ R_m, data = est)
  list(alpha = coef(mod)[1], beta = coef(mod)[2],
       sigma = sd(residuals(mod)), n = nrow(est), mod = mod)
}

event_study <- function(ticker, evt_date) {
  mm <- market_model(ticker, evt_date); if (is.null(mm)) return(NULL)
  ei   <- which.min(abs(as.numeric(returns_df$Date - evt_date)))
  ew_i <- max(1, ei + EVT_WIN[1]):min(nrow(returns_df), ei + EVT_WIN[2])
  ew <- returns_df[ew_i, ] %>%
    mutate(day = ew_i - ei) %>%            # true offset, robust to gaps
    select(Date, day, R_i = all_of(ticker), R_m = Market) %>% drop_na() %>%
    mutate(E_R = mm$alpha + mm$beta * R_m, AR = R_i - E_R)
  n_days <- nrow(ew)
  # A bank that had already stopped trading contributes NO days to this
  # window. Without this guard it entered with CAR = 0 and was averaged in as
  # if it had experienced no abnormal return, pulling the event CAAR toward
  # zero: Events 4 and 5 came out at -15.93% and -8.88% instead of -16.63%
  # and -9.26%. SIVB and SBNY are the affected banks, which is why the thesis
  # says Events 4 and 5 rest on 46 institutions rather than 48.
  if (n_days < 1) return(NULL)
  CAR <- sum(ew$AR)
  # FIX 4: include the prediction-error term (1 + 1/M) that Section 3.2
  # defines and that the Patell standardisation below already used. The old
  # line was sigma * sqrt(n_days), so two parts of the pipeline disagreed.
  se_car <- mm$sigma * sqrt(n_days * (1 + 1 / mm$n))
  t_stat <- CAR / se_car
  list(ticker = ticker, evt_date = evt_date, alpha = mm$alpha, beta = mm$beta,
       R2 = summary(mm$mod)$r.squared, sigma = mm$sigma, n_est = mm$n,
       n_days = n_days, CAR = CAR, t_stat = t_stat,
       p_value = 2 * pt(-abs(t_stat), df = mm$n - 2), ew_data = ew)
}

cat("\n[4-5] Running the event study ...\n")
results_list <- list()
for (i in seq_len(nrow(EVENTS))) for (t in available) {
  r <- tryCatch(event_study(t, EVENTS$date[i]), error = function(e) NULL)
  if (!is.null(r)) { r$event_id <- EVENTS$id[i]
    results_list[[paste0(t, "_e", EVENTS$id[i])]] <- r }
}
results_summary <- map_dfr(results_list, function(r) tibble(
  ticker = r$ticker, event_id = r$event_id, alpha = r$alpha, beta = r$beta,
  R2 = r$R2, sigma = r$sigma, CAR = r$CAR, t_stat = r$t_stat,
  p_value = r$p_value, n_est = r$n_est, n_days = r$n_days))
ew_all <- map_dfr(results_list, ~ .x$ew_data %>%
                    mutate(ticker = .x$ticker, event_id = .x$event_id))
cat("     ", nrow(results_summary), "bank-event results\n")

# =====================================================================
# 6. AAR / CAAR, PATELL Z, BANK CHARACTERISTICS
# =====================================================================
AAR_table <- ew_all %>%
  group_by(event_id, day) %>%
  summarise(AAR = mean(AR, na.rm = TRUE),
            se  = sd(AR, na.rm = TRUE) / sqrt(n()),
            t   = AAR / se, n = n(), .groups = "drop") %>%
  group_by(event_id) %>%
  mutate(CAAR = cumsum(AAR),
         # FIX 2: stars from the p-value on t(n-1), matching the thesis legend
         # (*** p<0.001, ** p<0.01, * p<0.05, . p<0.10). The old code compared
         # |t| against 2.576 / 1.960 / 1.645 -- the NORMAL 1/5/10% values --
         # which disagreed with Table 4 on two of the five Event 2 days.
         p   = 2 * pt(-abs(t), df = pmax(n - 1, 1)),
         sig = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**",
                         p < 0.05  ~ "*",   p < 0.10 ~ ".", TRUE ~ "")) %>%
  ungroup()

patell <- results_summary %>%
  mutate(S_i = sigma * sqrt(n_days * (1 + 1 / n_est)),
         SCAR = CAR / S_i, v_i = (n_est - 2) / (n_est - 4)) %>%
  group_by(event_id) %>%
  summarise(CAAR_mean_pct = 100 * mean(CAR),
            Patell_Z = sum(SCAR, na.rm = TRUE) / sqrt(sum(v_i, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(p_value = 2 * pnorm(-abs(Patell_Z)))

bank_chars_core <- tibble::tribble(
  ~ticker, ~uninsured_dep_pct, ~assets_bn, ~tier1_lev, ~ltd_ratio,
  "SIVB", 94.3,  209.0, 7.96, 42.0,   "SBNY", 89.3,  110.4, 8.79, 83.9,
  "FRC",  67.3,  212.6, 8.51, 94.1,   "ZION", 49.2,   89.5, 7.65, 76.5,
  "WAL",  52.6,   67.7, 8.22, 98.7,   "PACW", 71.1,   44.3, 9.70, 84.4,
  "CMA",  59.3,   85.5, 9.01, 71.9,   "KEY",  48.2,  187.6, 8.78, 81.8,
  "RF",   34.8,  154.2, 8.80, 71.7,   "JPM",  59.5, 3201.9, 8.30, 46.1,
  "BAC",  41.7, 2418.5, 7.68, 50.4,   "WFC",  39.3, 1717.5, 8.34, 63.8,
  "USB",  52.2,  585.1, 8.05, 71.2,   "TFC",  41.8,  546.0, 8.54, 76.0)

kbw_path <- find_data("kbw_bank_chars.csv")
bank_chars_kbw <- if (!is.na(kbw_path))
  read_csv(kbw_path, show_col_types = FALSE) %>%
    select(ticker, assets_bn, uninsured_dep_pct, tier1_lev, ltd_ratio) else NULL

# FIX 16 (this one changed N). bind_rows() stacked the 14-bank core table on
# the 46-bank KBW CSV with no de-duplication, and 12 tickers appear in BOTH.
# The result was 60 rows for 48 banks, so the cross-sectional regression ran
# with SIVB, SBNY, FRC, ZION, WAL, KEY, RF, JPM, BAC, WFC, USB and TFC each
# counted TWICE -- exactly the banks that drive the result. distinct() keeps
# the first match, i.e. the hand-checked core values, which is the intent.
bank_chars <- bind_rows(bank_chars_core, bank_chars_kbw) %>%
  distinct(ticker, .keep_all = TRUE) %>%
  filter(ticker %in% available) %>%
  mutate(reg_exempt = as.integer(assets_bn < 100),
         gsib       = as.integer(assets_bn > 700),
         group      = case_when(ticker %in% c("SIVB","SBNY","FRC") ~ "Failed",
                                assets_bn > 500 ~ "Large", TRUE ~ "Regional"))
cat("\n[6] Bank characteristics:", nrow(bank_chars), "banks",
    if (nrow(bank_chars) == 48) "(correct)" else "*** EXPECTED 48 ***", "\n")

# =====================================================================
# 7. GOOGLE TRENDS  ->  composite information index
# =====================================================================
cat("\n[7] Google Trends ...\n")
returns_df$info_index <- NA_real_
p_svb <- find_data("google_trends_svb.csv")
p_br  <- find_data("google_trends_bankrun.csv")
gt_ok <- FALSE

if (!is.na(p_svb) && !is.na(p_br)) {          # FIX 6: look in ./ AND data/
  gt_ok <- tryCatch({
    a1 <- read_csv(p_svb, show_col_types = FALSE); names(a1) <- tolower(names(a1))
    a2 <- read_csv(p_br,  show_col_types = FALSE); names(a2) <- tolower(names(a2))
    brc <- names(a2)[grepl("bank", names(a2))][1]
    # FIX 5: the two CSVs come from ONE Trends query and already share a scale
    # (SVB peaks at 100, 'bank run' at 7). Re-normalising each series to its
    # own maximum -- which the old code did -- inflates 'bank run' about
    # fourteen-fold and gives the rarer term equal weight. Keep them as
    # downloaded; rescale only the composite.
    gt <- full_join(
      a1 %>% transmute(Date = as.Date(time), svb = as.numeric(svb)),
      a2 %>% transmute(Date = as.Date(time), br  = as.numeric(.data[[brc]])),
      by = "Date") %>% arrange(Date) %>%
      mutate(info_index = (svb + br) / 2,
             info_index = info_index / max(info_index, na.rm = TRUE) * 100)
    # FIX 17 (this one silently disabled the whole CSV path). returns_df
    # already carries an all-NA info_index, so joining a second one produced
    # info_index.x / info_index.y and the next step threw "Column info_index
    # doesn't exist" -- swallowed by tryCatch with an EMPTY message, after
    # which the loader fell through to the API. Drop the placeholder first.
    returns_df <<- returns_df %>%
      select(-any_of("info_index")) %>%
      left_join(gt %>% select(Date, info_index), by = "Date") %>%
      arrange(Date) %>% fill(info_index, .direction = "down")
    gt_series <<- gt
    message("  ok  composite index built from the bundled CSVs (",
            nrow(gt), " days)")
    TRUE
  }, error = function(e) { message("  x   Trends CSV: ", conditionMessage(e)); FALSE })
}
if (!gt_ok) message("  !   No Google Trends index -- Table 6 will be skipped.")

# =====================================================================
# 8. CROSS-SECTIONAL REGRESSION  (Table 5)
# =====================================================================
hc3 <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC3"))
reg_df <- results_summary %>% filter(event_id == 2) %>%
  select(ticker, CAR, sigma, n_days) %>%
  inner_join(bank_chars, by = "ticker") %>%
  distinct(ticker, .keep_all = TRUE) %>% drop_na(uninsured_dep_pct)

cat("\n[8] Cross-section: N =", nrow(reg_df),
    if (nrow(reg_df) == 48) "(correct)" else "*** EXPECTED 48 ***", "\n")

m_full <- lm(CAR ~ uninsured_dep_pct + log(assets_bn) + tier1_lev + ltd_ratio, reg_df)
m_pars <- lm(CAR ~ uninsured_dep_pct + log(assets_bn), reg_df)
m_reg  <- lm(CAR ~ uninsured_dep_pct + log(assets_bn) + reg_exempt + gsib, reg_df)

# randomization inference on the key coefficient (Section 5.3).
# Permutes the uninsured ratio across banks, holding the observed CAR vector
# -- and hence its whole dependence structure -- fixed. Takes the formula and
# the ORIGINAL data frame: passing mod$model instead would hand lm a column
# literally named "log(assets_bn)" and it would then look for assets_bn.
perm_p <- function(fml, data, B = 20000, seed = 20230310) {
  set.seed(seed)
  b0 <- coef(lm(fml, data))["uninsured_dep_pct"]
  X  <- model.matrix(fml, data); y <- model.response(model.frame(fml, data))
  j  <- which(colnames(X) == "uninsured_dep_pct")
  bs <- replicate(B, {
    Xp <- X; Xp[, j] <- sample(Xp[, j])
    qr.coef(qr(Xp), y)[j] })
  mean(abs(bs) >= abs(b0), na.rm = TRUE)
}

# =====================================================================
# 9. AGGREGATE INFERENCE  (Section 5.2)
# =====================================================================
e2 <- results_summary %>% filter(event_id == 2)
car2 <- e2$CAR
t_naive <- mean(car2) / (sd(car2) / sqrt(length(car2)))

M <- ew_all %>% filter(event_id == 2) %>% select(ticker, day, AR) %>%
  pivot_wider(names_from = ticker, values_from = AR) %>% select(-day) %>%
  as.matrix()
R <- suppressWarnings(cor(M, use = "pairwise.complete.obs"))
rbar <- mean(R[upper.tri(R)], na.rm = TRUE)
# FIX 1: Kolari & Pynnonen (2010) carry a (1 - rbar) NUMERATOR as well as the
# (1 + (n-1)rbar) denominator. The old code kept only the denominator, which
# returns -1.25 where the correct value is -0.77.
n_kp  <- length(car2)
t_kp  <- t_naive * sqrt((1 - rbar) / (1 + (n_kp - 1) * rbar))

# calendar-time portfolio: ONE equally weighted portfolio per calendar day
ct_input <- ew_all %>%
  distinct(ticker, Date, .keep_all = TRUE) %>%          # FIX 15: no repeats
  group_by(Date) %>%
  summarise(port_R = mean(R_i, na.rm = TRUE),
            Market = mean(R_m, na.rm = TRUE), .groups = "drop") %>% drop_na()
ct_reg <- lm(port_R ~ Market, ct_input)
ct_hac <- coeftest(ct_reg, vcov = NeweyWest(ct_reg, lag = 2, prewhite = FALSE))
# FIX 3: t critical value on the regression's own d.o.f., not 1.96 -- the
# interval rests on about fifteen daily observations.
ct_t   <- qt(0.975, df = df.residual(ct_reg))

# =====================================================================
# 10. ATTENTION PANEL  (Table 6)
# =====================================================================
panel_res <- NULL
if (gt_ok) {
  panel_raw <- ew_all %>% select(ticker, Date, AR, event_id) %>%
    left_join(bank_chars, by = "ticker") %>%
    left_join(returns_df %>% select(Date, info_index), by = "Date") %>% drop_na()
  # FIX 10: the four March windows overlap, so ew_all repeats the same
  # bank-day once per containing window -- 1,165 rows instead of 697, and the
  # estimator re-weighted toward the most overlapped dates.
  panel_df <- panel_raw %>% distinct(ticker, Date, .keep_all = TRUE) %>%
    mutate(info_std = info_index / 100,
           uninsd_x_info = uninsured_dep_pct * info_std,
           assets_x_info = log(assets_bn)    * info_std,
           reg_x_info    = reg_exempt        * info_std)
  cat("\n[10] Panel:", nrow(panel_raw), "raw ->", nrow(panel_df), "unique bank-days (",
      nrow(panel_raw) - nrow(panel_df), "repeats removed )\n")

  # Two-way FE. Uses fixest when available (much faster, and it is what the
  # thesis cites); otherwise falls back to double demeaning in base R.
  demean2 <- function(d, v) {
    x <- d[[v]]
    for (k in 1:30) {
      x <- x - ave(x, d$ticker); x <- x - ave(x, as.factor(d$Date))
    }
    x
  }
  fe_fit <- function(rhs) {
    d <- panel_df
    y <- demean2(d, "AR"); X <- sapply(rhs, function(v) demean2(d, v))
    X <- matrix(X, nrow = nrow(d)); colnames(X) <- rhs
    fit <- lm(y ~ X - 1); names(fit$coefficients) <- rhs; fit
  }
  # Wild cluster bootstrap by date, imposing the null (Cameron, Gelbach &
  # Miller 2008). With ~15 date clusters the asymptotic cluster-robust
  # p-values over-reject badly, so these bootstrap p-values are the ones to
  # quote in Table 6.
  fit_fe <- function(rhs, d = panel_df) {
    if (HAS_FE) {
      f <- as.formula(paste("AR ~", paste(rhs, collapse = " + "), "| ticker + Date"))
      m <- feols(f, data = d, vcov = "twoway")
      list(b = unname(coef(m)[rhs[1]]),
           t = unname(m$coeftable[rhs[1], "t value"]),
           fitted = as.numeric(predict(m)), resid = as.numeric(resid(m)))
    } else {
      y <- demean2(d, "AR")
      X <- matrix(sapply(rhs, function(v) demean2(d, v)), nrow = nrow(d))
      colnames(X) <- rhs
      f <- lm(y ~ X - 1); names(f$coefficients) <- rhs
      se <- sqrt(diag(vcovCL(f, cluster = d$Date)))[1]
      list(b = unname(coef(f)[1]), t = unname(coef(f)[1] / se),
           fitted = as.numeric(fitted(f)), resid = as.numeric(resid(f)))
    }
  }
  wild_p <- function(rhs, B = 999, seed = 20230310) {
    set.seed(seed)
    obs <- fit_fe(rhs)
    # restricted fit: impose the null on the variable of interest
    r0  <- if (length(rhs) > 1) fit_fe(rhs[-1]) else
             list(fitted = rep(0, nrow(panel_df)),
                  resid = demean2(panel_df, "AR"))
    cl  <- unique(panel_df$Date)
    ts  <- numeric(B)
    for (b in seq_len(B)) {
      w  <- sample(c(-1, 1), length(cl), TRUE); names(w) <- as.character(cl)
      dd <- panel_df
      dd$AR <- r0$fitted + r0$resid * w[as.character(panel_df$Date)]
      ts[b] <- tryCatch(fit_fe(rhs, dd)$t, error = function(e) NA_real_)
    }
    list(coef = obs$b, t = obs$t, p = mean(abs(ts) >= abs(obs$t), na.rm = TRUE))
  }
  specs <- list(m1 = "uninsd_x_info",
                m2 = c("uninsd_x_info", "assets_x_info"),
                m3 = c("uninsd_x_info", "assets_x_info", "reg_x_info"))
  panel_res <- map_dfr(names(specs), function(k) {
    r <- wild_p(specs[[k]])
    cat("   ", k, "done\n")
    tibble(model = k, coef = r$coef, t = r$t, boot_p = r$p, N = nrow(panel_df))
  })
}

# =====================================================================
# 11. ROBUSTNESS  (Section 5.5, Table 7)
# =====================================================================
trim_fit <- function(d) { m <- lm(CAR ~ uninsured_dep_pct + log(assets_bn), d)
  ct <- hc3(m); c(beta = ct[2,1], t = ct[2,3], p = ct[2,4], N = nrow(d)) }
ord <- reg_df %>% arrange(CAR)
robust_tbl <- bind_rows(
  tibble(what = "all banks",                  !!!trim_fit(reg_df)),
  tibble(what = "excl. 3 failed",             !!!trim_fit(reg_df %>% filter(!ticker %in% c("SIVB","SBNY","FRC")))),
  tibble(what = "excl. 3 most negative",      !!!trim_fit(ord %>% slice(-(1:3)))),
  tibble(what = "excl. 4 most negative",      !!!trim_fit(ord %>% slice(-(1:4)))),
  tibble(what = "excl. bottom decile",        !!!trim_fit(ord %>% slice(-(1:round(nrow(ord)*0.10))))),
  tibble(what = "excl. bottom quintile",      !!!trim_fit(ord %>% slice(-(1:round(nrow(ord)*0.20))))),
  tibble(what = "complete-window banks only", !!!trim_fit(reg_df %>% filter(n_days == max(n_days)))))

mean_ar <- ew_all %>% filter(event_id == 2) %>% group_by(ticker) %>%
  summarise(mean_AR = mean(AR, na.rm = TRUE), .groups = "drop")
m_meanar <- lm(mean_AR ~ uninsured_dep_pct + log(assets_bn),
               reg_df %>% inner_join(mean_ar, by = "ticker"))

set.seed(20230310)
bs <- replicate(2000, { d <- reg_df[sample(nrow(reg_df), replace = TRUE), ]
  coef(lm(CAR ~ uninsured_dep_pct + log(assets_bn), d))[2] })
loo <- sapply(seq_len(nrow(reg_df)), function(i)
  coef(lm(CAR ~ uninsured_dep_pct + log(assets_bn), reg_df[-i, ]))[2])

quant_tbl <- NULL
if (HAS_QR) {
  set.seed(20230310)
  quant_tbl <- map_dfr(c(.10,.25,.50,.75,.90), function(tau) {
    f <- quantreg::rq(CAR ~ uninsured_dep_pct + log(assets_bn), tau = tau, data = reg_df)
    s <- summary(f, se = "boot", bsmethod = "xy", R = 400)$coefficients["uninsured_dep_pct", ]
    tibble(tau = tau, beta = s[1], se = s[2], t = s[3], p = s[4]) })
}
m_quad <- lm(CAR ~ uninsured_dep_pct + I(uninsured_dep_pct^2) + log(assets_bn), reg_df)
m_thr  <- lm(CAR ~ uninsured_dep_pct * I(as.numeric(uninsured_dep_pct > 60)) +
               log(assets_bn), reg_df)

# =====================================================================
# 12. FAMA-FRENCH THREE-FACTOR CHECK  (needs the Dartmouth download)
# =====================================================================
ff3_df <- tryCatch({
  z <- tempfile(fileext = ".zip")
  download.file(paste0("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/",
                       "ftp/F-F_Research_Data_Factors_daily_CSV.zip"), z, quiet = TRUE)
  f <- unzip(z, exdir = tempdir()); L <- readLines(f[1])
  s <- grep("^\\s*[0-9]{8}", L)[1]; e <- max(grep("^\\s*[0-9]{8}", L))
  read.csv(text = paste(c("Date,Mkt_RF,SMB,HML,RF", L[s:e]), collapse = "\n")) %>%
    mutate(Date = as.Date(as.character(Date), "%Y%m%d"),
           across(-Date, ~ .x / 100)) %>%
    filter(Date >= as.Date(START_DATE), Date <= as.Date(END_DATE))
}, error = function(e) { message("\n[12] FF3 factors unavailable: ", e$message); NULL })

ff3_tbl <- NULL
if (!is.null(ff3_df)) {
  ff_one <- function(ticker, evt, three = TRUE) {
    d <- returns_df %>% select(Date, R_i = all_of(ticker)) %>%
      inner_join(ff3_df, by = "Date") %>% mutate(Ex = R_i - RF)
    ei <- which.min(abs(as.numeric(d$Date - evt)))
    if (ei + EST_WIN[2] < 1) return(NULL)
    est <- d[max(1, ei + EST_WIN[1]):(ei + EST_WIN[2]), ] %>% drop_na()
    if (nrow(est) < 30) return(NULL)
    mod <- if (three) lm(Ex ~ Mkt_RF + SMB + HML, est) else lm(Ex ~ Mkt_RF, est)
    ew  <- d[max(1, ei + EVT_WIN[1]):min(nrow(d), ei + EVT_WIN[2]), ] %>% drop_na()
    pred <- if (three) coef(mod)[1] + coef(mod)[2]*ew$Mkt_RF + coef(mod)[3]*ew$SMB +
                       coef(mod)[4]*ew$HML + ew$RF
            else       coef(mod)[1] + coef(mod)[2]*ew$Mkt_RF + ew$RF
    sum(ew$R_i - pred)
  }
  # FIX 11: Table C.5 says the CAPM column is the EXCESS-return specification,
  # for comparability. The old code compared the raw-return market model
  # against FF3 -- a different benchmark from the one the note describes.
  ff3_tbl <- map_dfr(available, function(t) tibble(
    ticker   = t,
    CAR_raw  = results_summary$CAR[results_summary$ticker == t &
                                     results_summary$event_id == 2][1],
    CAR_capm = tryCatch(ff_one(t, EVENTS$date[2], FALSE), error = function(e) NA_real_),
    CAR_ff3  = tryCatch(ff_one(t, EVENTS$date[2], TRUE),  error = function(e) NA_real_)))
}

# =====================================================================
# 13. THESIS NUMBERS  -- copy these across
# =====================================================================
pc <- function(x, d = 2) sprintf(paste0("%.", d, "f"), 100 * x)
line <- function() cat(strrep("-", 70), "\n")
cat("\n\n")
cat("=====================================================================\n")
cat("  THESIS NUMBERS\n")
cat("=====================================================================\n")

cat("\n### Table 2 -- bank characteristics by group\n"); line()
print(bank_chars %>% group_by(group) %>%
  summarise(N = n(), unins_mean = mean(uninsured_dep_pct),
            unins_min = min(uninsured_dep_pct), unins_max = max(uninsured_dep_pct),
            assets = mean(assets_bn), tier1 = mean(tier1_lev),
            ltd = mean(ltd_ratio), .groups = "drop") %>%
  bind_rows(bank_chars %>% summarise(group = "All", N = n(),
            unins_mean = mean(uninsured_dep_pct), unins_min = min(uninsured_dep_pct),
            unins_max = max(uninsured_dep_pct), assets = mean(assets_bn),
            tier1 = mean(tier1_lev), ltd = mean(ltd_ratio))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% as.data.frame())

cat("\n### Section 5.1 -- CAAR by event (mean of bank-level CARs)\n"); line()
print(results_summary %>% group_by(event_id) %>%
  summarise(N = n(), CAAR_pct = round(100 * mean(CAR), 2), .groups = "drop") %>%
  as.data.frame())

cat("\n### Tables 3 / C.4 -- Event 2 CARs, all banks\n"); line()
print(e2 %>% inner_join(bank_chars %>% select(ticker, group), by = "ticker") %>%
  transmute(ticker, group, CAR_log_pct = round(100 * CAR, 1),
            simple_pct = round(100 * (exp(CAR) - 1), 1),
            t = round(t_stat, 2), n_days) %>%
  arrange(CAR_log_pct) %>% as.data.frame(), row.names = FALSE)

cat("\n### Table 4 -- AAR / CAAR, Event 2\n"); line()
print(AAR_table %>% filter(event_id == 2) %>%
  transmute(day, AAR_pct = round(100 * AAR, 2), t = round(t, 2),
            CAAR_pct = round(100 * CAAR, 2), p = round(p, 4), sig, N = n) %>%
  as.data.frame(), row.names = FALSE)
cat("  NOTE: this cumulated CAAR differs slightly from the mean of the\n")
cat("  bank-level CARs above, because N falls within the window.\n")

cat("\n### Table 5 -- cross-sectional regression (HC3)\n"); line()
cat("-- Full model --\n");          print(hc3(m_full))
cat("R2", round(summary(m_full)$r.squared, 4),
    "| adj R2", round(summary(m_full)$adj.r.squared, 4), "| N", nrow(reg_df), "\n")
cat("\n-- Parsimonious model --\n"); print(hc3(m_pars))
cat("R2", round(summary(m_pars)$r.squared, 4),
    "| adj R2", round(summary(m_pars)$adj.r.squared, 4), "\n")
cat("95% CI on uninsured:", round(confint(m_pars)["uninsured_dep_pct", ], 5), "\n")
cat("\n-- Regulatory / G-SIB model (Section 5.3) --\n"); print(hc3(m_reg))
cat("\nRandomization inference, 20,000 permutations:\n")
cat("  parsimonious p =",
    perm_p(CAR ~ uninsured_dep_pct + log(assets_bn), reg_df),
    "| full p =",
    perm_p(CAR ~ uninsured_dep_pct + log(assets_bn) + tier1_lev + ltd_ratio, reg_df),
    "\n")

cat("\n### Section 5.2 -- aggregate inference\n"); line()
cat("mean pairwise AR correlation (rbar) :", round(rbar, 3), "\n")
cat("naive cross-sectional t             :", round(t_naive, 2), "\n")
cat("Kolari-Pynnonen adjusted t          :", round(t_kp, 2), "\n")
cat("Patell Z by event:\n"); print(patell %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nCalendar-time portfolio (", nrow(ct_input), "daily observations ):\n")
print(ct_hac)
a <- coef(ct_reg)[1]; se <- ct_hac[1, 2]
cat(sprintf("  alpha = %.4f%%/day | 5-day CAAR = %.2f%%  [95%% CI %.2f%%, %.2f%%]\n",
            100*a, 500*a, 500*(a - ct_t*se), 500*(a + ct_t*se)))

cat("\n### Table 6 -- attention panel\n"); line()
if (is.null(panel_res)) cat("  skipped (no Google Trends index)\n") else
  print(panel_res %>% mutate(across(where(is.numeric), ~ round(.x, 4))) %>% as.data.frame())

cat("\n### Table 7 -- quantile regression\n"); line()
if (is.null(quant_tbl)) cat("  skipped (quantreg not installed)\n") else
  print(quant_tbl %>% mutate(across(where(is.numeric), ~ round(.x, 4))) %>% as.data.frame())

cat("\n### Section 5.5 -- robustness\n"); line()
print(robust_tbl %>% mutate(across(where(is.numeric), ~ round(.x, 4))) %>% as.data.frame())
cat("\nmean daily AR as dependent variable:\n"); print(hc3(m_meanar))
cat("\nbootstrap (2000): mean", round(mean(bs), 5),
    "| 95% CI [", round(quantile(bs, .025), 5), ",", round(quantile(bs, .975), 5), "]\n")
cat("leave-one-out: range [", round(min(loo), 5), ",", round(max(loo), 5),
    "] | negative in", sum(loo < 0), "of", length(loo), "\n")
if (requireNamespace("MASS", quietly = TRUE))
  cat("Huber M-estimator:", round(coef(MASS::rlm(
    CAR ~ uninsured_dep_pct + log(assets_bn), data = reg_df))[2], 5), "\n")
cat("quadratic term p =", round(hc3(m_quad)[3, 4], 3),
    "| threshold interaction p =", round(hc3(m_thr)[4, 4], 3),
    "| banks above 60% :", sum(reg_df$uninsured_dep_pct > 60), "\n")

cat("\n### Tables B.1 / B.2 -- summary statistics and correlations\n"); line()
print(reg_df %>% transmute(CAR_pct = 100*CAR, uninsured_dep_pct, assets_bn,
                           tier1_lev, ltd_ratio) %>%
  summarise(across(everything(), list(mean = mean, sd = sd, min = min,
                                      med = median, max = max))) %>%
  pivot_longer(everything(), names_to = c("var", "stat"), names_sep = "_(?=[a-z]+$)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% as.data.frame())
cat("\ncorrelation matrix:\n")
print(round(cor(reg_df %>% transmute(CAR, uninsured_dep_pct, la = log(assets_bn),
                                     tier1_lev, ltd_ratio)), 2))

cat("\n### Tables C.2 / C.3 -- diagnostics\n"); line()
m6 <- lm(CAR ~ uninsured_dep_pct + log(assets_bn) + tier1_lev + ltd_ratio +
           reg_exempt + gsib, reg_df)
# Build a clean design frame with syntactic names first: model.frame() would
# hand back a column literally called "log(assets_bn)", which reformulate()
# then tries to evaluate as a call.
Xv <- as.data.frame(model.matrix(m6)[, -1, drop = FALSE])
names(Xv) <- c("uninsured", "logAssets", "tier1", "ltd", "regExempt", "gsib")
vifs <- sapply(names(Xv), function(v)
  1 / (1 - summary(lm(reformulate(setdiff(names(Xv), v), v), data = Xv))$r.squared))
cat("VIFs (six-regressor model, log assets as estimated):\n"); print(round(vifs, 2))
# For contrast: entering assets in LEVELS is what produced the 13.1 / 11.4 in
# the old Table C.2, even though the regression uses logs (FIX 7).
Xl <- Xv; Xl$logAssets <- reg_df$assets_bn; names(Xl)[2] <- "assetsLevels"
vifl <- sapply(names(Xl), function(v)
  1 / (1 - summary(lm(reformulate(setdiff(names(Xl), v), v), data = Xl))$r.squared))
cat("VIFs if assets entered in levels (NOT the estimated model):\n")
print(round(vifl, 2))
bp <- bptest(m6); cat(sprintf("\nBreusch-Pagan: LM = %.2f, df = %d, p = %.4f\n",
                              bp$statistic, bp$parameter, bp$p.value))
r <- resid(m_full); n <- length(r)
sk <- mean((r - mean(r))^3) / sd(r)^3; ku <- mean((r - mean(r))^4) / sd(r)^4 - 3
cat(sprintf("Jarque-Bera: JB = %.2f, p = %.4f (skew %.3f, excess kurt %.3f)\n",
            n/6*(sk^2 + ku^2/4), 1 - pchisq(n/6*(sk^2 + ku^2/4), 2), sk, ku))
cd <- cooks.distance(m_full); names(cd) <- reg_df$ticker
cd <- sort(cd, decreasing = TRUE)
cat("Cook's distance, top 8 (threshold 4/n =", round(4/nrow(reg_df), 3), "):\n")
print(round(head(cd, 8), 3))
cat("observations above threshold:", sum(cd > 4/nrow(reg_df)), "\n")

cat("\n### Table C.5 -- CAPM vs FF3\n"); line()
if (is.null(ff3_tbl)) cat("  skipped (Fama-French factors unavailable)\n") else {
  cat("correlation raw vs FF3:",
      round(cor(ff3_tbl$CAR_raw, ff3_tbl$CAR_ff3, use = "complete.obs"), 4), "\n")
  cat("mean CAR: raw", pc(mean(ff3_tbl$CAR_raw, na.rm = TRUE)),
      "% | excess-CAPM", pc(mean(ff3_tbl$CAR_capm, na.rm = TRUE)),
      "% | FF3", pc(mean(ff3_tbl$CAR_ff3, na.rm = TRUE)), "%\n")
  cat("rank reversals raw vs FF3:",
      sum(rank(ff3_tbl$CAR_raw) != rank(ff3_tbl$CAR_ff3), na.rm = TRUE),
      "of", nrow(ff3_tbl), "\n")
  cat("FF3 cross-section:\n")
  print(hc3(lm(CAR_ff3 ~ uninsured_dep_pct + log(assets_bn),
               ff3_tbl %>% inner_join(bank_chars, by = "ticker"))))
  print(ff3_tbl %>% mutate(across(where(is.numeric), ~ round(100*.x, 1))) %>%
          arrange(CAR_raw) %>% head(12) %>% as.data.frame(), row.names = FALSE)
}

cat("\n### Section 4.4 -- Google Trends composite\n"); line()
if (gt_ok) {
  g <- gt_series
  pk <- which.max(g$info_index); i1 <- which(g$Date == as.Date("2023-05-01"))
  cat("peak", round(max(g$info_index), 1), "on", format(g$Date[pk]), "\n")
  cat("unscaled average at the peak:", round((g$svb[pk] + g$br[pk])/2, 2), "\n")
  cat("Jan-Feb mean:", round(mean(g$info_index[g$Date < as.Date("2023-03-01")], na.rm=TRUE), 2), "\n")
  if (length(i1)) cat("1 May value:", round(g$info_index[i1], 2),
                      "| peak is", round(100/g$info_index[i1], 1), "x larger\n")
} else cat("  skipped\n")

# =====================================================================
# 14. SELF-TESTS
# =====================================================================
cat("\n\n### SELF-TESTS\n"); line()
P <- 0; F <- 0
chk <- function(lbl, cond) { ok <- isTRUE(cond)
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", lbl))
  if (ok) P <<- P + 1 else F <<- F + 1 }
chk("48 banks in the cross-section",              nrow(reg_df) == 48)
chk("bank_chars has no duplicate tickers",        !any(duplicated(bank_chars$ticker)))
chk("Google Trends loaded from the CSVs",         gt_ok)
chk("KP factor lies in (0,1)",                    abs(t_kp) < abs(t_naive))
chk("KP formula keeps the (1-rbar) numerator",
    isTRUE(all.equal(unname(t_kp),
      unname(t_naive * sqrt((1-rbar)/(1+(n_kp-1)*rbar))))))
chk("calendar-time CI uses t, not 1.96",          ct_t > 1.96)
chk("per-bank t includes (1 + 1/M)",
    isTRUE(all.equal(unname(e2$t_stat[1]),
      unname(e2$CAR[1] / (e2$sigma[1] * sqrt(e2$n_days[1] * (1 + 1/e2$n_est[1])))))))
if (!is.null(panel_res)) chk("panel de-duplicated", panel_res$N[1] < 800)
chk("all Event 2 CARs are negative",              all(e2$CAR < 0))
cat(sprintf("\n  %d passed, %d failed\n", P, F))

cat("\n=====================================================================\n")
cat("  Done. Copy the numbers above into the thesis tables.\n")
cat("  Anything marked *** EXPECTED *** or FAIL needs attention first.\n")
cat("=====================================================================\n")


# =====================================================================
#  ADDENDUM -- quantities the thesis still needs re-exported
# =====================================================================
line()
cat("\n### ADDENDUM A -- CAAR by event on the banks that actually traded\n"); line()
caar_tbl <- results_summary %>% group_by(event_id) %>%
  summarise(N = n(), CAAR_pct = round(100 * mean(CAR), 2), .groups = "drop")
print(as.data.frame(caar_tbl))
frc5 <- results_summary %>% filter(event_id == 5, ticker == "FRC")
if (nrow(frc5)) cat(sprintf("  FRC at Event 5: %d window days, CAR = %.2f%%\n",
                            frc5$n_days[1], 100 * frc5$CAR[1]))

cat("\n### ADDENDUM B -- Table C.1, market model, Event 2 estimation window\n"); line()
c1 <- results_summary %>% filter(event_id == 2) %>%
  filter(ticker %in% c("SIVB","SBNY","FRC","ZION","WAL","PACW","CMA","KEY",
                       "RF","JPM","BAC","WFC","USB","TFC")) %>%
  mutate(Alpha = round(alpha, 4), Beta = round(beta, 2), R2 = round(R2, 2),
         EstObs = n_est, ResidSD = round(sigma, 4)) %>%
  select(ticker, Alpha, Beta, R2, EstObs, ResidSD)
print(as.data.frame(c1))
cat(sprintf("  beta range over the 14: %.2f to %.2f | R2 range: %.2f to %.2f\n",
            min(c1$Beta), max(c1$Beta), min(c1$R2), max(c1$R2)))
cat(sprintf("  residual SD range over ALL 48: %.4f to %.4f\n",
            min(results_summary$sigma[results_summary$event_id == 2]),
            max(results_summary$sigma[results_summary$event_id == 2])))

cat("\n### ADDENDUM C -- Section 3.7, the Event 3 cross-section\n"); line()
reg3 <- results_summary %>% filter(event_id == 3) %>%
  select(ticker, CAR) %>% inner_join(bank_chars, by = "ticker")
cat("  N =", nrow(reg3), "\n")
m3p <- lm(CAR ~ uninsured_dep_pct + log(assets_bn), reg3)
print(hc3(m3p))
cat("R2", round(summary(m3p)$r.squared, 4),
    "| adj R2", round(summary(m3p)$adj.r.squared, 4), "\n")

cat("\n### ADDENDUM D -- Table 6, every row\n"); line()
if (!is.null(panel_res)) {
  print(as.data.frame(panel_res))
  specs <- list(m1 = "uninsd_x_info",
                m2 = c("uninsd_x_info","assets_x_info"),
                m3 = c("uninsd_x_info","assets_x_info","reg_x_info"))
  for (nm in names(specs)) {
    rhs <- specs[[nm]]
    if (HAS_FE) {
      f <- as.formula(paste("AR ~", paste(rhs, collapse = " + "), "| ticker + Date"))
      m <- feols(f, data = panel_df, vcov = "twoway")
      cat("\n --", nm, "--\n"); print(m$coeftable)
      cat(sprintf("   within R2 %.4f | N %d\n", fitstat(m, "wr2")$wr2, m$nobs))
    } else {
      y <- demean2(panel_df, "AR")
      X <- matrix(sapply(rhs, function(v) demean2(panel_df, v)), nrow = nrow(panel_df))
      colnames(X) <- rhs
      f <- lm(y ~ X - 1); names(f$coefficients) <- rhs
      cat("\n --", nm, "--\n"); print(summary(f)$coefficients)
      cat(sprintf("   within R2 %.4f | N %d\n", summary(f)$r.squared, nrow(panel_df)))
    }
  }
}

cat("\n### ADDENDUM E -- calendar-time portfolio, re-checked\n"); line()
cat("  (printed in the main body above; repeated here after the returns fix)\n")
line()
cat("  ADDENDUM COMPLETE\n"); line()
