> **Corrected edition.** Nineteen bugs have been fixed across this project;
> six of them changed reported numbers. The pipeline as published here
> reproduces the figures in the thesis. Every change is documented in
> `FIXES.md` and marked with a `# FIX` comment at the line it touches
> (`grep -rn "# FIX" R/ *.R`), and a base-R test suite guards each one:
>
> ```
> Rscript 13_test_fixes.R      # 54 checks, no packages or network needed
> ```
>
> The two that mattered most were the Kolari–Pynnönen adjustment, which had
> lost the `(1 - rbar)` numerator, and the returns frame, which differenced a
> merged price matrix and so dropped the return across First Republic's
> two-day trading halt — the largest single observation in the study and the
> whole content of Event 5. `FIXES.md` explains both and lists what the
> corrected run produces.

# Information Dynamics and Bank Runs — Event Study (2023 Banking Crisis)

Complete, runnable R project for the Master's thesis "Information Dynamics
and Bank Runs: An Event Study of the 2023 Banking Crisis" (Diyorbek
Ahmadjonov, University of Bonn, supervisor: Dr. Lorens Imhof).

Sample: 48 U.S. bank holding companies, including the three that failed
(SVB Financial / SIVB, Signature Bank / SBNY, First Republic / FRC) and
two that were severely repriced but survived (PacWest / PACW, Comerica /
CMA).

## HOW TO RUN

**Option A — one file, recommended.**
Open `thesis-event-study.Rproj` in RStudio, then:

```r
source("RUN_EVERYTHING.R")
```

`THESIS_ANALYSIS.R` is a second, self-contained route to the same numbers: a
single script that runs the whole analysis and prints every figure the thesis
reports, table by table. It is the script the reported results were exported
from. Use it if you want the output in one pass without sourcing the modules.

This installs any missing packages, prepares the PacWest/Comerica price
files from the two raw downloads described below, and runs the full
pipeline end to end. Edit the two file paths at the top of the script
first if your raw downloads aren't in `~/Downloads`.

**Option B — step by step.**

```r
source("R/00_packages.R")          # first time only
source("00_prepare_pacw_cma.R")    # needs the two raw files, see README.txt
source("run_all.R")                # the analysis, R/00 ... R/11 in order
source("12_verify_n48.R")          # optional: checks output against the thesis
```

Needs an internet connection: prices for 43 of the 48 banks download live
from Yahoo Finance / Stooq. Five are delisted or otherwise unavailable
from those sources and are bundled or prepared locally instead:
SIVB, SBNY, FRC (delisted, CSVs bundled at the project root), and PACW,
CMA (prepared from raw downloads by `00_prepare_pacw_cma.R` — see
`README.txt` for exact sources and instructions).

## WHAT IS INCLUDED

Every file listed below sits at the project root. The loaders also accept
a `data/` subfolder, and `00_prepare_pacw_cma.R` creates one for the two
files it writes, but nothing here has to be moved.

```
SIVB.csv, SBNY.csv, FRC.csv       delisted bank prices
google_trends_svb.csv             daily search interest, 'SVB'
google_trends_bankrun.csv         daily search interest, 'bank run'
README.txt                        exact data sources + PACW/CMA setup
kbw_bank_chars.csv                FDIC Q4 2022 characteristics, 46 banks
                                   (PACW/CMA characteristics are hard-coded
                                   in R/06_aar_caar_characteristics.R)
R/00 ... R/11                     the analysis, in run order
00_prepare_pacw_cma.R             converts raw PACW/CMA downloads to the
                                   format R/02_data_download.R expects
12_verify_n48.R                   reproduces the thesis's key reported
                                   numbers and flags MATCH/MISMATCH
13_test_fixes.R                   54 checks guarding the nineteen fixes
FIXES.md                          what each fix was and what it changed
RUN_EVERYTHING.R                  single-file version of all of the above
THESIS_ANALYSIS.R                 the script the reported results came from
```

## R/11_corrections.R — the corrected results

Runs automatically at the end of `run_all.R` and prints, between the
`====` markers, robustness numbers the thesis reports directly:

- **Panel fixed effects**: one observation per bank-date (the overlapping
  March windows previously entered the same bank-day more than once),
  plus a wild cluster bootstrap by date — the correct inference with
  only about 15 date clusters.
- **Calendar-time portfolio**: a time-series regression of the portfolio
  return on the market return, with Newey-West standard errors.
- **Event-window length**: halted banks contribute fewer trading days,
  so the cross-sectional regression is re-run on complete-window banks
  and on a length-invariant dependent variable.

## Verifying the thesis's numbers

`12_verify_n48.R` recomputes the event study, the cross-sectional
regression (Table 5), the quantile regression (Table 7), the Section 5.5
robustness checks, and daily AAR/CAAR with the Kolari-Pynnönen
correlation adjustment, then prints each result next to the number the
thesis reports, flagged MATCH or MISMATCH. As of the last run, all
checks matched, including PacWest and Comerica.
