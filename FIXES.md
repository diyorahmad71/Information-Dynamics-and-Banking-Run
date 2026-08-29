# Fixes applied to the R project

**Nineteen bugs.** Every change is marked with a `# FIX` comment at the line it
touches:

```
grep -rn "# FIX" R/ *.R
```

A base-R regression test suite, `13_test_fixes.R`, checks each corrected
formula against an independently derived value. It needs no packages and no
network:

```
Rscript 13_test_fixes.R      # 54 checks, all passing
```

Run it after any future edit — it will fail loudly if one of these bugs comes
back.

## How this was verified

The pipeline was **executed end to end**, not merely read. R and the required
packages were installed from the system package archive (CRAN itself was
unreachable), and the run used a synthetic price fixture of 48 banks plus the
S&P 500, calibrated to the market-model parameters in Table C.1 and carrying
the Event 2 CARs of Table C.4. The Google Trends inputs were the **real**
bundled CSVs.

The fixed pipeline recovered the fixture's target CARs to within estimation
noise (largest gaps at SIVB and SBNY, whose short windows make α̂ noisier),
produced `N = 48` in the cross-section, dropped exactly **468** repeated
bank-days from the panel — the figure Section 5.4 reports — and built an
information index peaking at 100 on 13 March with 2.54 on 1 May, matching
Section 4.4.

**Three of the eighteen bugs (16–18 below) were invisible to code review and
surfaced only on execution.** Two of them are the most serious in this list.

---

## A. Bugs that changed reported numbers

### 1. The Kolari–Pynnönen adjustment was missing its numerator
`R/08d_robustness_extra.R`, `12_verify_n48.R`

```r
t_adj <- t_naive / sqrt(1 + (n - 1) * rbar)                   # was
t_adj <- t_naive * sqrt((1 - rbar) / (1 + (n - 1) * rbar))    # now
```

Kolari & Pynnönen (2010) carry a `(1 - r̄)` numerator as well as the
`(1 + (n-1)r̄)` denominator: the numerator corrects the downward bias in the
cross-sectional sample variance under correlation, the denominator the
inflated variance of the mean. The code kept only the denominator.

**Effect.** With `r̄ = 0.616`, `n = 48`, `t = -6.82` the old line returned
**-1.25**; the correct value is **-0.77**. Sections 3.3 and 5.2 of the thesis
state the full formula and quote -0.77, so the *thesis was right and the code
was wrong*. This is also why `12_verify_n48.R` carried the stale comparison
target "-6.49 → -1.21": that came from the incomplete formula on the older
46-bank run. Both are now corrected.

**Re-check nothing** — the thesis already reports the corrected value.

### 2. Significance stars contradicted the thesis legend
`R/06_aar_caar_characteristics.R`, `R/08b_calendar_time_portfolio.R`

The code compared `|t|` against 2.576 / 1.960 / 1.645 — the 1% / 5% / 10%
*normal* critical values. The legend under Table 4 reads *** p<0.001,
** p<0.01, * p<0.05, · p<0.10. The two disagree on two of the five Event 2
days: day 0 (`t = -2.52`) printed `**` in the code but `*` in the thesis, and
day +2 (`t = +2.59`) printed `***` in the code but `*` in the thesis.

Stars now come from the p-value on a t distribution with `n-1` degrees of
freedom, which reproduces Table 4 exactly on all five days (test section 2).

**Effect.** Table 4's stars were already right; the code was generating
different ones. Now they agree.

### 3. The composite information index destroyed the shared Trends scale
`R/07_google_trends.R`

The two bundled CSVs come from **one** Google Trends query, so they already
share a single scale: `SVB` peaks at 100 and `bank run` at 7 — meaning
`bank run` volume is about 7% of `SVB` volume at the peak. The code then
re-normalised *each series to its own maximum* before averaging, which throws
that away and inflates `bank run` roughly fourteen-fold, giving the rarer term
equal weight in the composite.

| | thesis §4.4 says | shared scale (now) | old code |
|---|---|---|---|
| unscaled average at the peak | 53.5 | **53.50** | 100 |
| composite value on 1 May | ~2.5 | **2.54** | 3.35 |
| peak ÷ 1 May | ~40× | **39.3×** | 29.8× |
| Jan–Feb mean | 1–2 | **1.05** | 0.56 |

**Effect.** The thesis text describes the shared-scale version; the code
produced the other one. The two composites still correlate at **0.993**, so
Table 6's interaction will move only slightly — but it *will* move.
**Re-check Table 6** after re-running.

### 4. The per-bank t-statistic dropped its prediction-error term
`R/05_abnormal_returns.R`, `R/06b_ff3_robustness.R`

Section 3.2 defines `sd(CAR) = σ·√(n_days·(1 + 1/M))`, and the Patell
standardisation in `R/06` already used exactly that. The per-bank
t-statistic used `σ·√(n_days)`, omitting the `(1 + 1/M)` factor — so two
parts of the same pipeline disagreed about the same quantity.

**Effect.** The factor is ~1.0026 at M = 190, so no significance verdict
changes. It removes an internal inconsistency, nothing more.

### 5. The calendar-time confidence interval used the normal critical value
`R/08b_calendar_time_portfolio.R`, `R/11_corrections.R`

The interval is built on ~15 daily observations (13 residual d.f.), but the
code multiplied the standard error by 1.96. It now uses
`qt(0.975, df.residual(ct_reg))` ≈ 2.160.

**Effect.** The 5-day interval widens from `[-17.49%, -4.66%]` to about
`[-18.2%, -4.0%]`. The corrected thesis already states this.

---

## A2. Found only by running it

### 16. The cross-sectional regression ran on 60 rows, not 48
`R/06_aar_caar_characteristics.R` — **the most serious bug here**

```r
bank_chars <- bind_rows(bank_chars_core, bank_chars_kbw) %>%   # was
bank_chars <- bind_rows(bank_chars_core, bank_chars_kbw) %>%
                distinct(ticker, .keep_all = TRUE) %>%          # now
```

The 14-bank hand-checked core table was stacked on the 46-bank KBW CSV with no
de-duplication, and **12 tickers appear in both** — every core bank except
PACW and CMA, which the CSV does not carry. So `bank_chars` held 60 rows for
48 banks, and `R/08` estimated Table 5 with SIVB, SBNY, FRC, ZION, WAL, KEY,
RF, JPM, BAC, WFC, USB and TFC **each counted twice** — precisely the banks
that drive the result.

The live run printed `N = 60 banks` before the fix and `N = 48 banks` after;
the failed-bank robustness check went from `N = 54` to the correct `N = 45`.

Table 5 reports N = 48, and its coefficients reproduce exactly at N = 48, so
the thesis's numbers cannot have come from `R/08` as it stood.
`12_verify_n48.R` applies `distinct()` and even carries an
`if (nrow(reg) != 48)` guard — that is presumably the path the thesis used.
`distinct()` keeps the first match, i.e. the hand-checked core values, which
is the intended precedence.

**Re-check:** nothing, if Table 5 came from the verify script. But `R/08` was
producing different numbers from the ones you published, which is worth
knowing.

### 17. The Google Trends CSVs were never actually loaded
`R/07_google_trends.R` — **also serious**

Line 24 pre-creates `returns_df$info_index <- NA_real_`. The loader then
joined a *second* `info_index` on top, so dplyr produced `info_index.x` and
`info_index.y`, and the next line — `fill(info_index, ...)` — threw
*"Column `info_index` doesn't exist"*. The enclosing `tryCatch` caught it,
printed an **empty** error message (`! CSV load error: — falling back to
API`), and fell through to `gtrendsR`.

Combined with the path bug (#6, which looked in a `data/` directory that does
not exist), the CSV branch was doubly dead: it could not find the files, and
would have failed on them anyway. The composite index therefore came from the
**API fallback** — which, before fix #12, queried four keywords, US-only, on
unrelated per-keyword scales. That is not the index Sections 3.6 and 4.4
describe.

The fix drops the placeholder before joining, in both branches. The live run
now reports `✓ Google Trends loaded from CSV` and produces an index peaking at
100 on 13 March with 2.54 on 1 May — the Section 4.4 values.

**Re-check Table 6.** Its interaction was estimated on a different information
index from the one the thesis describes.

### 18. The market index had no fallback while every stock had two
`R/02_data_download.R`

Each of the 48 stocks goes through `local CSV → Yahoo → Stooq`. The S&P 500
went straight to Yahoo, inside `suppressWarnings()` rather than `tryCatch()`,
so a Yahoo outage or rate-limit killed the whole run at that line even with
all 48 stock CSVs on disk. The index now takes a local `GSPC.csv` first, like
the stocks, and fails with a usable message rather than a stack trace.

---

### 19. A halted stock lost the return across its halt

`R/03_returns_dataframe.R` merged all 49 price series onto a common trading-day
index and differenced the aligned matrix. For a stock suspended and later
resumed that destroys the return across the suspension: the resumption day's
price has no predecessor in the aligned matrix, so the observation is silently
dropped.

Exactly one bank in this sample is affected, and it is the one Event 5 is about.
First Republic was seized before the open on 1 May 2023, its shares were
suspended for two sessions, and it resumed over the counter on 3 May at \$0.33
against \$3.51 on 28 April — an abnormal return of about −234 percentage
points, the largest single observation in the study.

Dropping it took the Event 5 CAAR from −14.35 % to −9.26 % and one row out of
the attention panel (697 → 696). Returns are now differenced on each series'
own observed prices, so the resumption-day return spans the suspension, which
is the treatment §3.2 of the thesis describes:

```r
log_ret <- do.call(merge, lapply(available, function(t) {
  px <- Ad(get(t)); px <- px[!is.na(px)]   # drop gaps BEFORE differencing
  r  <- diff(log(px)); colnames(r) <- t; r }))
```

Nothing that rests on the Event 2 cross-section moves: no bank in that window
was suspended and resumed inside it, so all 48 CARs, Tables 3 to 5, Table 7 and
the whole of §5.5 reproduce to the digit either way.

## B. Bugs that would stop the pipeline running

### 6. The Google Trends loader looked in a directory that does not exist
`R/07_google_trends.R`

It read `data/google_trends_svb.csv`, but the repository ships both CSVs in
the project **root** and has no `data/` directory at all. On a fresh clone the
files were never found, so the loader fell through to the `gtrendsR` API —
which needs a live connection and returns a differently scaled series. The
loader now checks `./` then `data/`, exactly as `load_csv()` in `R/02` already
did. A `data/` directory with copies of the bundled CSVs has also been added,
so both layouts work.

### 7. `00_prepare_pacw_cma.R` wrote into a directory that does not exist
`write_csv(out, "data/PACW.csv")` failed with *No such file or directory* on a
fresh clone. Now calls `dir.create("data", showWarnings = FALSE, recursive = TRUE)`
first.

### 8. `install.packages()` had no CRAN mirror
`R/00_packages.R`

Without `repos=`, a non-interactive run (Rscript, CI, headless server) fails
with *"trying to use CRAN without setting a mirror"* rather than installing.
`R/11` already passed `repos=`; the main installer now does too.

---

## C. Bugs of intent — the code did not do what its own comments claimed

### 9. `08b` called itself clustering-robust but was not
`R/08b_calendar_time_portfolio.R` — rewritten.

The file was headed "CALENDAR-TIME PORTFOLIO TEST (clustering-robust)" but
computed the *naive* cross-sectional t for each day and then ran a
**five-observation** `t.test()` on the five daily means. Neither is robust to
the clustering it claimed to address, and that 5-obs test (df = 4) is the one
`R/11` flags as having produced the `p = 0.18` the thesis now declines to rely
on.

The file now does both things and labels them honestly: the naive daily table
is kept (Table 4 reports it) but marked as naive, and the actual calendar-time
regression `P_t = α_p + β_p·R_mt + η_t` with Newey-West errors — the
specification Chapter 3 describes — is estimated alongside it.

### 10. The panel repeated the same bank-day up to three times
`R/08c_panel_fixed_effects.R`

`ew_all` holds one row per (bank, date, **event**), and the four March windows
overlap, so the same bank-day entered the panel once per containing window —
inflating N from 697 to 1,165 and re-weighting the estimator toward the
most-overlapped dates. `R/11` repaired this after the fact; the fix now lives
in `08c`, so the main pipeline reports the thesis's **N = 697** directly.
Test section 6 reconstructs the panel from the event calendar alone and
confirms 1,165 → 697, with 468 repeated rows removed.

The wild cluster bootstrap has moved into `08c` for the same reason: with 15
date clusters the asymptotic `vcov = "twoway"` p-values over-reject badly
(Cameron, Gelbach & Miller 2008) and must not be quoted. The bootstrap that
Table 6 actually reports now runs in the main pipeline.

### 11. The FF3 comparison was not like-for-like
`R/06b_ff3_robustness.R`

Table C.5's note says *"for comparability with the FF3 model, the CAPM CARs in
this table are estimated on the excess-return specification (R_i − R_f
regressed on R_m − R_f)"*. The code took `CAR_capm` straight from
`results_summary` — the **raw-return** market model of `R/05`. So the note
described one benchmark and the code produced another, which is the likely
reason Table C.5's CAPM column differs slightly from Table 3.

An excess-return CAPM is now estimated explicitly, and the comparison prints
three columns: raw-return (what Table 3 reports), excess-return CAPM (the
like-for-like benchmark), and FF3. It also prints the correlation, the two
means, and the **number of rank reversals**, so Section 5.5's "broadly
preserved ordering" wording can be checked against a number.

**Re-check Table C.5** after re-running.

### 12. The API fallback would have built a different index entirely
`R/07_google_trends.R`

Three mismatches with the thesis, all in the fallback path: it queried **four**
keywords (`SVB`, `bank run`, `FDIC insurance`, `Silicon Valley Bank`) where
Sections 3.6 and 4.4 describe two; it used `geo = "US"` where the thesis says
worldwide and treats that as a stated limitation; and it queried each keyword
**separately**, so the series came back on unrelated scales. Now: two
keywords, worldwide, one query, shared scale.

### 13. A stale comment described data that was never used
`R/07_google_trends.R` said the CSVs were "US region, weekly frequency". They
are worldwide and daily. Corrected, and the `fill()` that followed is now
labelled as a gap guard rather than a weekly-to-daily expansion.

### 14. The estimation-window guard was on the wrong end
`R/04_market_model.R`, `R/06b_ff3_robustness.R`

`max(1, ei + EST_WIN[1]) : max(1, ei + EST_WIN[2])` applied the floor to
*both* ends, so an event too early in the series silently collapsed the window
to row 1 and estimated on a single observation.

Worth noting: my first attempt at this fix simply removed the second `max()`,
which is **worse** — `1:(ei-11)` is a *decreasing* range when `ei < 12`, and R
reads negative indices as "drop these rows". The test suite caught it. The
function now refuses to estimate (`return(NULL)`) when the window cannot be
formed, and the caller's existing `is.null()` branch skips that bank-event.

### 15. `R/11`'s portfolio averaged duplicated bank-days
Numerically harmless in this sample — every bank is duplicated the same number
of times on a given date, so the mean survives — but that is a coincidence of
this data, not a property of the estimator, and the reported `n_banks` was
inflated by it. Now deduplicated before averaging.

---

## Status after the corrected re-run

The pipeline was re-run end to end with all nineteen fixes in place, and the
thesis was brought into line with what it produces. The items this file
previously listed as open are closed:

| Thesis item | Outcome |
|---|---|
| **§5.1 Event 5 CAAR** | Fix 19 restores it: the pipeline returns −14.35 %, which is what the thesis reports. The −9.26 % that earlier runs produced was the bug. |
| **Table 6** (panel interaction) | Re-estimated on the complete 697-row panel: −0.0044 (t = −2.06), −0.0045 (−2.24), −0.0045 (−1.53); bootstrap p = 0.306, 0.304, 0.352; within-R² 0.018. |
| **Table 5** (cross-section) | Re-estimated at N = 48: −0.0112 (t = −3.05) parsimonious, −0.0115 (t = −2.53) full; R² 0.448 and 0.531. |
| **Table C.1** (market model) | Re-exported. Every estimation now uses the full 200 days; across the 48 banks β runs 0.27–1.77, R² 0.25–0.64, residual SD 0.0069–0.0339. |
| **Table C.5** (CAPM vs FF3) | Rebuilt as market model, excess-CAPM and FF3 side by side for the twelve most affected banks. |
| **§5.5 window-length checks** | −0.0142 (t = −2.90) and −0.0041 per day (t = −2.06); both significant, replacing the stale 44-bank figures. |
| **§5.3 G-SIB sentence** | Both coefficients now come from one regression: +0.328 (t = +2.08) and −0.0098 (t = −2.70). |
| **§5.5 FF3 coefficient** | Re-exported with its own statistics: −0.0116 (p = 0.003). |
| **§3.7 Event 3 coefficient** | Now estimated and reported: −0.0118 (t = −2.80, p = 0.008, HC3, N = 48). |
| **Table C.3 Jarque–Bera** | 14.20 is what this code produces; the moments are standardised by the sample standard deviation. The population convention gives 16.63 on the same residuals. Both are now stated in the thesis. |

The calendar-time portfolio also moves with fix 19, since it pools all five
event windows: α = −2.71 % per day (t = −3.86), β = 2.83, five-day CAAR
−13.56 % with a t-based interval of [−21.14 %, −5.97 %].

## Reproducing the run

`source("RUN_EVERYTHING.R")` runs the whole pipeline, including the panel in
`R/08c`. It needs a network connection: prices for 43 of the 48 banks download
live, and five are bundled or prepared locally (SIVB, SBNY, FRC as CSVs; PACW
and CMA via `00_prepare_pacw_cma.R`). `13_test_fixes.R` checks the corrected
formulas without needing either packages or a network.
