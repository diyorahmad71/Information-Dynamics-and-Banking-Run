# =====================================================================
# 01_settings_events.R
# Global settings, date windows, and the five-event definition.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 1. SETTINGS ─────────────────────────────────────────────
START_DATE <- "2022-04-01"   # extended so the [-210,-11] window is genuine
END_DATE   <- "2023-06-30"
 
# Original 14 core banks: failed / regional contagion / large controls
TICKERS_CORE <- c(
  "SIVB", "SBNY", "FRC",
  "ZION", "WAL",  "PACW", "CMA",  "KEY",  "RF",
  "JPM",  "BAC",  "WFC",  "USB",  "TFC"
)
 
# KBW Regional Banking Index / KRE ETF expansion (~35 additional tickers)
# Source: KBW Nasdaq Regional Banking Index (KRX) constituents, March 2023
TICKERS_KBW <- c(
  "FITB", "HBAN", "CFG",  "FHN",  "SNV",  "COLB", "OZK",
  "WTFC", "BOKF", "UMBF", "VLY",  "NYCB", "WBS",  "HWC",
  "EWBC", "CBSH", "FFBC", "PB",   "TCBI", "ONB",  "UBSI",
  "GBCI", "HTLF", "INDB", "WSFS", "BANR", "FFIN", "TRMK",
  "SFNC", "HAFC", "CVBF", "RNST", "VBTX", "WAFD", "CATY",
  "EBC",  "GABC", "FBMS", "HTBK", "IBCP"
)
 
# Combined ticker list (core + KBW expansion)
TICKERS <- c(TICKERS_CORE, TICKERS_KBW)
 
# 5 key crisis events
EVENTS <- tribble(
  ~id, ~name,                              ~date,
  1,   "SVB bond loss announcement",       "2023-03-08",
  2,   "SVB & Signature Bank closure",     "2023-03-10",
  3,   "FDIC systemic risk exception",     "2023-03-12",
  4,   "First Republic share collapse",    "2023-03-15",
  5,   "First Republic seizure",           "2023-05-01"
) %>% mutate(date = as.Date(date))
 
EST_WIN <- c(-210, -11)   # estimation window (trading days)
EVT_WIN <- c(-2,   +2)    # event window
