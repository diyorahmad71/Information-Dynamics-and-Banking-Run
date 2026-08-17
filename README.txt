DATA FILES USED BY THE SCRIPTS
==============================
The loader in R/02_data_download.R tries, for each ticker, in order:
  1. a local CSV in this data/ folder (or the project root): TICKER.csv
  2. Yahoo Finance (still-listed tickers)
  3. Stooq (free; keeps DELISTED tickers), with retries for its daily limit
A local CSV always wins, so dropping a file here makes that ticker
reproducible and immune to rate limits.

Yahoo-format columns expected in any CSV you place here:
  Date, Open, High, Low, Close, Adj Close, Volume

1. Delisted / retired tickers -- provide as CSV if the automatic
   Stooq fallback is rate-limited:
     - SIVB.csv, SBNY.csv, FRC.csv   (failed banks)
     - PACW.csv                       (PacWest Bancorp -- see note below)
     - CMA.csv                        (only if Yahoo keeps missing Comerica)
   Free sources for the delisted ones:
     Stooq        https://stooq.com/q/d/?s=pacw.us   ("Download data")
     Investing.com  search "PacWest Bancorp" -> Historical Data
   Date range: Sep 1 2022 -> Jun 30 2023.

   >>> IMPORTANT (PACW): PacWest merged INTO Banc of California on
   Nov 30 2023 and now trades as BANC. That is AFTER this study's window,
   so use PacWest's OWN history under the retired ticker PACW/pacw.us.
   Do NOT substitute BANC (Banc of California) prices -- pre-merger BANC
   is a different, smaller company and would be the wrong security.

2. Google Trends weekly series (for Figure 1):
     - google_trends_svb.csv       (column: "svb")
     - google_trends_bankrun.csv   (column: "Bank run")
   Exported from trends.google.com (US, Jan-Jun 2023). If absent and the
   Trends API is unreachable, Figure 1 is skipped; all else still runs.

--------------------------------------------------------------------
N = 48 RE-RUN (added August 2026)
--------------------------------------------------------------------
To bring PacWest and Comerica into the analysis you need two more
price files in this folder:

  data/PACW.csv   and   data/CMA.csv

Do NOT create these by hand. Run, from the project root:

  source("00_prepare_pacw_cma.R")

It converts the raw investing.com and MacroTrends downloads into the
Yahoo-style layout that R/02_data_download.R expects, and prints a
coverage check.

Then:
  source("run_all.R")
  source("12_verify_n48.R")

Bank characteristics for PACW and CMA are ALREADY hard-coded in
R/06_aar_caar_characteristics.R (bank_chars_core), so nothing needs
adding there.
