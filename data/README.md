DATA FILES USED BY THE SCRIPTS
==============================
The loader in R/02_data_download.R tries, for each ticker, in order:
  1. a local CSV in the project root (or in a data/ subfolder): TICKER.csv
  2. Yahoo Finance (still-listed tickers)
  3. Stooq (free; keeps DELISTED tickers), with retries for its daily limit
A local CSV always wins, so dropping a file beside this one makes that
ticker reproducible and immune to rate limits.

Yahoo-format columns expected in any CSV you add:
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
PacWest and Comerica are COMMITTED as data/PACW.csv and data/CMA.csv, so a
fresh clone reproduces all 48 banks with no manual downloads.

They were built by 00_prepare_pacw_cma.R from the raw exports named
above, which live in data/raw/ and are not redistributed. Run that
script only if you want to rebuild them from source.

Bank characteristics for PACW and CMA are ALREADY hard-coded in
R/06_aar_caar_characteristics.R (bank_chars_core), so nothing needs
adding there.

--------------------------------------------------------------------
WBS.csv -- WEBSTER FINANCIAL (added September 2026)
--------------------------------------------------------------------
data/WBS.csv is committed because Yahoo Finance repeatedly refused to
return this ticker. It failed twice on 6 September 2026, and each time
the run silently fell to 47 banks instead of 48, which moved the
bottom-quintile p-value from 0.911 to 0.567 and the panel from 697 to
682 rows. Committing the series makes the sample deterministic.

Source: Investing.com -> "Webster Financial" -> Historical Data,
2022-03-01 to 2023-10-31, converted into the Yahoo column layout. The
raw export is kept at:

  data/raw/Webster Financial Stock Price History.csv

(Files in data/raw/ are gitignored and not redistributed.)

CAVEAT, same as PACW: these are split-adjusted but NOT
dividend-adjusted closes, whereas the thesis's original WBS series came
from Yahoo's Adj Close. No ex-dividend date falls inside 8-15 March
2023, so the Event 2 CAR is unaffected, but alpha and beta are
estimated over ~200 days that do contain ex-dividend dates and may
differ marginally from the published estimates.

CHECK AFTER RUNNING: Table C.4 of the thesis reports WBS at -22.0%.
If a run returns a materially different figure, the price source is not
equivalent to the original and the Yahoo series should be preferred.
