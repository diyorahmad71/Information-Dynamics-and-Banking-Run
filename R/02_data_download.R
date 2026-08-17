# =====================================================================
# 02_data_download.R
# Download stock prices (Yahoo) and Fama-French 3 factors.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 2. DATA DOWNLOAD ─────────────────────────────────────────
# Robust loader: for each ticker try, in order,
#   (1) a local CSV (TICKER.csv or data/TICKER.csv)  -- deterministic, no rate limits
#   (2) Yahoo Finance via quantmod (with retries)    -- still-listed tickers
#   (3) Stooq (with retries)                          -- keeps DELISTED tickers (e.g. PACW)
# This ordering means a manually-downloaded CSV always wins, so once you drop
# PACW.csv / CMA.csv in data/ the pipeline is reproducible and rate-limit-proof.

STOOQ_TRIES <- 3      # Stooq caps free CSV pulls per day/IP; retry with backoff
YAHOO_TRIES <- 2      # guard against transient Yahoo hiccups (e.g. CMA)

# Helper: load a local Yahoo-format CSV (checks ./ and data/)
load_csv <- function(ticker) {
  cand <- c(paste0(ticker, ".csv"), file.path("data", paste0(ticker, ".csv")))
  f <- cand[file.exists(cand)][1]
  if (is.na(f)) return(FALSE)
  tryCatch({
    df <- read_csv(f, show_col_types = FALSE,
                   comment = "#", na = c("", "NA", "null", "N/A")) %>%
      mutate(
        Date     = as.Date(Date),
        AdjClose = suppressWarnings(as.numeric(`Adj Close`))
      ) %>%
      filter(!is.na(AdjClose),
             Date >= as.Date(START_DATE),
             Date <= as.Date(END_DATE)) %>%
      arrange(Date)
    if (nrow(df) < 10) return(FALSE)
    mat <- matrix(rep(df$AdjClose, 6), ncol = 6)
    px  <- xts(mat, order.by = df$Date)
    colnames(px) <- paste0(ticker,
      c(".Open", ".High", ".Low", ".Close", ".Volume", ".Adjusted"))
    assign(ticker, px, envir = .GlobalEnv)
    message("  ✓ ", ticker, " — local CSV ", f, " (", nrow(df), " rows)")
    TRUE
  }, error = function(e) {
    message("  ✗ ", ticker, " CSV error: ", e$message); FALSE
  })
}

# Helper: Stooq (free, includes delisted). Retries handle the daily hit-limit.
load_stooq <- function(ticker) {
  url <- paste0("https://stooq.com/q/d/l/?s=", tolower(ticker), ".us",
                "&d1=", gsub("-", "", START_DATE),
                "&d2=", gsub("-", "", END_DATE), "&i=d")
  for (attempt in seq_len(STOOQ_TRIES)) {
    ok <- tryCatch({
      df <- suppressWarnings(read_csv(url, show_col_types = FALSE))
      # Stooq returns an HTML/text error (e.g. "Exceeded the daily hits limit")
      # instead of a CSV when throttled: detect and retry.
      need <- c("Date", "Open", "High", "Low", "Close")
      if (!all(need %in% names(df)) || nrow(df) < 10) stop("bad/empty Stooq response")
      df <- df %>%
        mutate(Date = as.Date(Date)) %>%
        filter(Date >= as.Date(START_DATE), Date <= as.Date(END_DATE)) %>%
        arrange(Date)
      if (nrow(df) < 10) stop("too few rows in window")
      vol <- if ("Volume" %in% names(df)) df$Volume else rep(NA_real_, nrow(df))
      # NOTE: Stooq's basic download is not dividend-adjusted, so Adjusted = Close.
      # For close-to-close log returns over a 5-day window this is immaterial.
      px <- xts(cbind(df$Open, df$High, df$Low, df$Close, vol, df$Close),
                order.by = df$Date)
      colnames(px) <- paste0(ticker,
        c(".Open", ".High", ".Low", ".Close", ".Volume", ".Adjusted"))
      assign(ticker, px, envir = .GlobalEnv)
      message("  ✓ ", ticker, " — Stooq (", nrow(df), " rows)")
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(1.5 * attempt)   # backoff before retry
  }
  FALSE
}

# Helper: Yahoo via quantmod, with retries for transient failures.
load_yahoo <- function(ticker) {
  for (attempt in seq_len(YAHOO_TRIES)) {
    ok <- tryCatch({
      suppressWarnings(getSymbols(ticker, src = "yahoo",
                                  from = START_DATE, to = END_DATE,
                                  auto.assign = TRUE, env = globalenv()))
      if (nrow(get(ticker)) < 10) stop("too few rows")
      message("  ✓ ", ticker, " — Yahoo Finance")
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(1.0 * attempt)
  }
  FALSE
}

# Download all tickers: local CSV -> Yahoo -> Stooq
message("=== Downloading stock data ===")
available <- character(0)
for (t in TICKERS) {
  ok <- load_csv(t) || load_yahoo(t) || load_stooq(t)
  if (!isTRUE(ok)) message("  ✗ ", t, " — no source succeeded")
  if (isTRUE(ok)) available <- c(available, t)
  Sys.sleep(0.5)   # be polite to the data hosts
}

# S&P 500 market index
suppressWarnings(
  getSymbols("^GSPC", src = "yahoo",
             from = START_DATE, to = END_DATE,
             auto.assign = TRUE)
)
message("✓ ^GSPC — S&P 500 market index\n")
 
missing_tickers <- setdiff(TICKERS, available)
cat("=== Download Summary ===\n")
cat("Available (", length(available), "):", paste(available, collapse = " "), "\n")
if (length(missing_tickers) > 0)
  cat("MISSING   (", length(missing_tickers), "):",
      paste(missing_tickers, collapse = " "), "\n")
cat("========================\n\n")
 
if (length(available) < 5)
  stop("Too few tickers available. Download the CSVs and re-run.")
 
 
# ── 2b. FAMA-FRENCH 3 FACTORS ────────────────────────────────
message("Downloading Fama-French 3 daily factors...")
ff3_df <- tryCatch({
  ff3_url <- paste0(
    "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/",
    "ftp/F-F_Research_Data_Factors_daily_CSV.zip"
  )
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempdir()
  download.file(ff3_url, tmp_zip, quiet = TRUE, mode = "wb")
  ff3_csv <- unzip(tmp_zip, exdir = tmp_dir)
 
  raw   <- readLines(ff3_csv[1])
  start <- which(grepl("^\\s*\\d{8}", raw))[1]
  ends  <- which((raw == "" | grepl("Copyright", raw,
                  ignore.case = TRUE)) & seq_along(raw) > start)
  end   <- if (length(ends) > 0) ends[1] - 1 else length(raw)
 
  read.csv(
    text       = paste(raw[start:end], collapse = "\n"),
    header     = FALSE,
    col.names  = c("date","Mkt_RF","SMB","HML","RF"),
    strip.white = TRUE
  ) %>%
    as_tibble() %>%
    mutate(
      Date = as.Date(as.character(date), "%Y%m%d"),
      across(c(Mkt_RF, SMB, HML, RF),
             ~ suppressWarnings(as.numeric(.)) / 100)
    ) %>%
    filter(!is.na(Date),
           Date >= as.Date(START_DATE),
           Date <= as.Date(END_DATE)) %>%
    select(Date, Mkt_RF, SMB, HML, RF)
}, error = function(e) {
  message("  ! FF3 download failed: ", e$message)
  NULL
})
if (!is.null(ff3_df))
  message("  ✓ FF3 factors loaded (", nrow(ff3_df), " trading days)\n")
