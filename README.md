# Information Dynamics and Bank Runs — An Event Study of the 2023 Banking Crisis

Replication package for the Master's thesis:

> **Information Dynamics and Bank Runs: An Event Study of the 2023 Banking Crisis**
> Diyorbek Ahmadjonov · M.Sc. Economics · Rheinische Friedrich-Wilhelms-Universität Bonn
> Supervisor: Dr. Lorens Imhof · Submitted September 2026

The thesis itself is in this repository: [`thesis.pdf`](thesis.pdf).

**Sample.** 48 publicly traded U.S. bank holding companies from the KBW Nasdaq Regional
Banking Index and the largest constituents of the KBW Nasdaq Bank Index, including the
three banks that failed (SIVB, SBNY, FRC) and two severely repriced survivors (PACW, CMA).

**Method.** Market-model event study over five crisis events in March–May 2023; a
cross-sectional regression of Event 2 CARs on FDIC Q4 2022 balance-sheet characteristics;
Patell standardisation; a Kolari–Pynnönen cross-sectional-dependence adjustment; a
calendar-time portfolio; quantile regression; and a two-way fixed-effects panel.

---

## Layout

```
README.md                  this file
FIXES.md                   the 19 bugs fixed, and how each was verified
LICENSE                    MIT (code)
CITATION.cff               how to cite
thesis.pdf                 the submitted thesis
thesis-event-study.Rproj   RStudio project (sets the working directory)

RUN_EVERYTHING.R           one command: whole pipeline + transcript to output/RUN_LOG.txt
run_all.R                  step-by-step runner, sources R/00 … R/11 in order
00_prepare_pacw_cma.R      builds data/PACW.csv and data/CMA.csv from raw downloads
12_verify_n48.R            recomputes the thesis's numbers, prints MATCH / MISMATCH
13_test_fixes.R            54 regression tests on the corrected formulas (base R, no network)
THESIS_ANALYSIS.R          single-file version of the whole analysis

R/                         the pipeline, in run order
  00_packages.R            package installation
  01_settings_events.R     event dates, estimation and event windows
  02_data_download.R       price loader: local CSV -> Yahoo -> Stooq
  03_returns_dataframe.R   log returns
  04_market_model.R        OLS market model over [-210, -11]      (Appendix A.1)
  05_abnormal_returns.R    ARs and CARs over [-2, +2]             (Appendix A.2)
  06_aar_caar_characteristics.R  AAR/CAAR, Patell Z, bank characteristics (A.3)
  06b_ff3_robustness.R     Fama-French three-factor robustness
  07_google_trends.R       attention series and Figure 1
  08_cross_sectional_regression.R  HC3 cross-section              (Appendix A.4)
  08b_calendar_time_portfolio.R    calendar-time portfolio        (Appendix A.4)
  08c_panel_fixed_effects.R  two-way FE panel, wild cluster bootstrap
  08d_robustness_extra.R   leave-one-out, bootstrap, Kolari-Pynnonen
  09_plots.R               figures
  10_save_results.R        writes result tables
  11_corrections.R         panel / calendar-time / window-length corrections

data/
  README.md                data provenance and download instructions
  SIVB.csv SBNY.csv FRC.csv  delisted failed banks
  google_trends_svb.csv      daily search interest, 'SVB'
  google_trends_bankrun.csv  daily search interest, 'bank run'
  kbw_bank_chars.csv         FDIC Q4 2022 characteristics, 46 banks
  raw/                       put the raw PACW/CMA downloads here

output/                    every generated table, figure and the run transcript
```

PacWest and Comerica characteristics are hard-coded in `R/06_aar_caar_characteristics.R`
and match Table B.3 of the thesis exactly (PACW 71.1 / 44.3 / 9.70 / 84.4; CMA
59.3 / 85.5 / 9.01 / 71.9).

---

## Running it

Requires R (≥ 4.2). Open `thesis-event-study.Rproj` in RStudio so the working directory is
the project root, then:

```r
source("RUN_EVERYTHING.R")
```

First run only, to build the two price files that are not committed:

```r
source("00_prepare_pacw_cma.R")
```

That needs two raw downloads in `data/raw/` — see [`data/README.md`](data/README.md) for
the exact sources. Without them `12_verify_n48.R` stops immediately.

### Check the sample size before trusting a run

Prices for most banks download live from Yahoo Finance, with Stooq as a fallback, so a run
depends on those services being up. **Exactly six tickers are expected to fail** — SNV,
NYCB, HTLF, VBTX, FBMS, HTBK — which is what Table 5's note in the thesis refers to. The
cross-section should then report:

```
N = 48 banks
```

If you see `N = 47` (or fewer), a ticker that should have loaded did not. Check the
download summary for an unexpected `✗`. A run of 2026-09-06, for example, lost **WBS**
(Webster Financial) to a transient Yahoo failure, which changed the bottom-quintile
p-value from 0.911 to 0.567 and the panel from 697 to 682 rows. The fix is to re-run, or
to download the missing ticker from Stooq (`https://stooq.com/q/d/?s=wbs.us`, 2022-03-01
to 2023-06-30) and save it as `data/WBS.csv` in Yahoo's column layout.

### Verifying against the thesis

`12_verify_n48.R` recomputes the reported quantities and prints each next to the published
value, flagged MATCH or MISMATCH. `13_test_fixes.R` is a 54-check regression suite over the
corrected formulas; it needs no packages and no network:

```
Rscript 13_test_fixes.R
```

`FIXES.md` documents the nineteen bugs these tests guard against.

---

## Data sources

| Source | Used for |
|---|---|
| [Yahoo Finance](https://finance.yahoo.com/) | daily adjusted closing prices |
| [Stooq](https://stooq.com/) | fallback, and delisted tickers |
| [FDIC BankFind Suite API](https://banks.data.fdic.gov/api/) | Q4 2022 call-report characteristics |
| [Kenneth R. French Data Library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html) | Fama-French daily three factors |
| [Google Trends](https://trends.google.com/) | daily search interest |

Exact queries and date ranges: [`data/README.md`](data/README.md).

---

## License

Code under the MIT License ([`LICENSE`](LICENSE)). Thesis text and figures © 2026 Diyorbek
Ahmadjonov, all rights reserved. Price and call-report data remain subject to their
providers' terms and are included only as needed to reproduce the published results.
