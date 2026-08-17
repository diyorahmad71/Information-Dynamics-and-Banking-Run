# =====================================================================
# 00_prepare_pacw_cma.R
#
# STEP 1 OF 2.  Run this BEFORE run_all.R.
#
# Converts the two raw price files you downloaded into the Yahoo-style
# CSVs that R/02_data_download.R expects, and writes them to data/.
# Once data/PACW.csv and data/CMA.csv exist, the pipeline's load_csv()
# helper picks them up automatically (local CSV wins over Yahoo/Stooq),
# so PACW and CMA enter the analysis exactly like every other bank.
#
# HOW TO RUN
#   1. Open thesis-event-study.Rproj in RStudio.
#   2. Put your two raw files somewhere you can point at, and set the
#      two paths below.
#   3. source("00_prepare_pacw_cma.R")
#   4. Read the diagnostics it prints. Do not proceed if it warns.
#   5. Then source("run_all.R")
#
# IMPORTANT CAVEAT, PLEASE READ
#   The PacWest file from investing.com gives the CLOSING price, which
#   is adjusted for splits but NOT for dividends. Every other bank in
#   your sample uses Yahoo's Adj Close, which IS dividend-adjusted.
#   PacWest paid a quarterly dividend through 2022, so its estimation-
#   window returns are very slightly understated relative to the rest
#   of the sample. The effect on a five-day event-window CAR is
#   negligible (no ex-dividend date falls inside 8-15 March 2023), but
#   alpha and beta are estimated on ~200 days that do contain ex-div
#   dates. This is a real, if small, inconsistency and you should
#   disclose it in Section 4.1 rather than leave it silent.
# =====================================================================

# ---- SET THESE TWO PATHS ---------------------------------------------
PACW_RAW <- "~/Downloads/PacWest Stock Price History.csv"
CMA_RAW  <- "~/Downloads/MacroTrends_Data_Download_CMA.csv"
# ----------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr)
})

if (!dir.exists("data")) dir.create("data")

# helper: strip thousands separators and quotes, coerce to numeric
num <- function(x) suppressWarnings(as.numeric(gsub("[,\"$]", "", trimws(x))))

write_yahoo <- function(df, ticker) {
  stopifnot(all(c("Date", "Close") %in% names(df)))
  out <- df %>%
    filter(!is.na(Date), !is.na(Close), Close > 0) %>%
    arrange(Date) %>%
    transmute(Date,
              Open      = ifelse(is.na(Open),  Close, Open),
              High      = ifelse(is.na(High),  Close, High),
              Low       = ifelse(is.na(Low),   Close, Low),
              Close     = Close,
              `Adj Close` = Close,
              Volume    = ifelse(is.na(Volume), 0, Volume))
  f <- file.path("data", paste0(ticker, ".csv"))
  write_csv(out, f)
  cat(sprintf("\n  wrote %s  |  %d rows  |  %s to %s\n",
              f, nrow(out), min(out$Date), max(out$Date)))
  out
}

# ---------------------------------------------------------------------
# PACW  --  investing.com layout:
#   "Date","Price","Open","High","Low","Vol.","Change %"
#   Date is MM/DD/YYYY, rows are newest-first, values are quoted.
# ---------------------------------------------------------------------
cat("\n=== PACW ===")
pacw_raw <- read.csv(PACW_RAW, stringsAsFactors = FALSE,
                     check.names = FALSE, fileEncoding = "UTF-8-BOM")
cat("\n  raw columns:", paste(names(pacw_raw), collapse = " | "))
cat("\n  raw rows:", nrow(pacw_raw), "\n")

if (!all(c("Date", "Price") %in% names(pacw_raw)))
  stop("PACW file does not have the expected 'Date' and 'Price' columns. ",
       "Columns found: ", paste(names(pacw_raw), collapse = ", "))

pacw <- data.frame(
  Date   = as.Date(pacw_raw$Date, format = "%m/%d/%Y"),
  Open   = num(pacw_raw$Open),
  High   = num(pacw_raw$High),
  Low    = num(pacw_raw$Low),
  Close  = num(pacw_raw$Price),
  Volume = NA_real_
)
pacw_out <- write_yahoo(pacw, "PACW")

# ---------------------------------------------------------------------
# CMA  --  MacroTrends layout: ~14 lines of header text, then
#   date,open,high,low,close,volume
# ---------------------------------------------------------------------
cat("\n=== CMA ===")
cma_lines <- readLines(CMA_RAW, warn = FALSE)
hdr <- grep("^date,open,high,low,close,volume", cma_lines)
if (length(hdr) == 0)
  stop("Could not find the 'date,open,high,low,close,volume' header line ",
       "in the CMA file. Open it and check the format.")
cat("\n  header found on line", hdr[1], "\n")

cma_raw <- read.csv(text = paste(cma_lines[hdr[1]:length(cma_lines)],
                                 collapse = "\n"),
                    stringsAsFactors = FALSE)
cma <- data.frame(
  Date   = as.Date(cma_raw$date),
  Open   = num(cma_raw$open),
  High   = num(cma_raw$high),
  Low    = num(cma_raw$low),
  Close  = num(cma_raw$close),
  Volume = num(cma_raw$volume)
)
# keep only what the pipeline window needs, plus slack
cma <- cma %>% filter(Date >= as.Date("2022-01-01"),
                      Date <= as.Date("2023-12-31"))
cma_out <- write_yahoo(cma, "CMA")

# ---------------------------------------------------------------------
# Diagnostics: does each series actually cover the estimation window?
# The market model needs ~200 trading days ending 11 days before the
# event. For Event 2 (2023-03-10) that reaches back to roughly May 2022.
# ---------------------------------------------------------------------
cat("\n\n=== COVERAGE CHECK ===\n")
need_from <- as.Date("2022-05-05")   # ~210 trading days before 2023-03-10
for (nm in c("PACW", "CMA")) {
  d <- get(paste0(tolower(nm), "_out"))
  pre  <- sum(d$Date >= as.Date("2022-04-01") & d$Date < as.Date("2023-02-23"))
  ok   <- min(d$Date) <= need_from
  cat(sprintf("  %-5s first obs %s  (need <= %s)  %s | ~%d obs in estimation range\n",
              nm, min(d$Date), need_from,
              ifelse(ok, "OK", "*** TOO LATE ***"), pre))
  if (!ok)
    warning(nm, ": series starts after the estimation window opens. ",
            "alpha/beta will be estimated on fewer than 200 days and ",
            "04_market_model.R will print a SHORT DATA warning.")
  if (pre < 150)
    warning(nm, ": only ", pre, " observations in the estimation range. ",
            "Expect a SHORT DATA warning.")
}

# Did the event window itself trade?
cat("\n=== EVENT 2 WINDOW (8-15 March 2023) ===\n")
for (nm in c("PACW", "CMA")) {
  d <- get(paste0(tolower(nm), "_out"))
  w <- d %>% filter(Date >= as.Date("2023-03-08"), Date <= as.Date("2023-03-15"))
  cat(sprintf("  %-5s %d trading days present:\n", nm, nrow(w)))
  print(w %>% select(Date, Close))
}

cat("\nDone. If nothing above says TOO LATE or throws a warning,",
    "\nnow run:  source(\"run_all.R\")\n",
    "\nthen:     source(\"12_verify_n48.R\")\n\n")
