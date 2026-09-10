# =====================================================================
# 06_aar_caar_characteristics.R
# AAR, CAAR, corrected full Patell test, bank characteristics.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 6. AAR & CAAR ────────────────────────────────────────────
ew_all <- map_dfr(results_list, ~ .x$ew_data %>%
  mutate(ticker = .x$ticker, event_id = .x$event_id))
 
AAR_table <- ew_all %>%
  group_by(event_id, day) %>%
  summarise(AAR  = mean(AR, na.rm = TRUE),
            se   = sd(AR, na.rm = TRUE) / sqrt(n()),
            t    = AAR / se,
            n    = n(), .groups = "drop") %>%
  group_by(event_id) %>%
  mutate(CAAR = cumsum(AAR),
         # FIX (significance stars). The old code compared |t| against
         # 2.576 / 1.960 / 1.645 -- the 1% / 5% / 10% NORMAL critical values --
         # but the thesis legend for Table 4 reads *** p<0.001, ** p<0.01,
         # * p<0.05, . p<0.10. The two disagree: day 0 (t = -2.52) printed
         # "**" here but "*" in the thesis, and day +2 (t = +2.59) printed
         # "***" here but "*" in the thesis. Stars are now derived from the
         # p-value on a t distribution with n-1 df, which reproduces Table 4.
         p    = 2 * pt(-abs(t), df = pmax(n - 1, 1)),
         sig  = case_when(p < 0.001 ~ "***",
                          p < 0.01  ~ "**",
                          p < 0.05  ~ "*",
                          p < 0.10  ~ ".",
                          TRUE      ~ "")) %>%
  ungroup() %>%
  mutate(p = round(p, 4),
         across(c(AAR, se, CAAR), ~ round(.x * 100, 3)))
 
cat("\n=== AAR / CAAR TABLE ===\n")
print(AAR_table)
 
# Full Patell (1976) standardised-residual test (replaces simplified sum(CAR/sigma)).
# SCAR_i = CAR_i / [sigma_i * sqrt(L_i*(1 + 1/M_i))];  Var(SCAR_i) = (M_i-2)/(M_i-4).
patell <- results_summary %>%
  mutate(S_i  = sigma * sqrt(n_days * (1 + 1 / n_est)),
         SCAR = CAR / S_i,
         v_i  = (n_est - 2) / (n_est - 4)) %>%
  group_by(event_id) %>%
  summarise(CAAR_pct = paste0(round(mean(CAR) * 100, 2), "%"),
            Patell_Z = round(sum(SCAR, na.rm = TRUE) / sqrt(sum(v_i, na.rm = TRUE)), 3),
            p_value  = round(2 * pnorm(-abs(sum(SCAR, na.rm = TRUE) /
                                            sqrt(sum(v_i, na.rm = TRUE)))), 4),
            .groups  = "drop")
cat("\n=== PATELL Z-STATISTICS ===\n")
print(patell)
 
 
 
# ── UPGRADE 1: BANK CHARACTERISTICS (Q4 2022) ────────────────────────────────
# Strategy:
#   Step A — Hard-coded values for the original 14 core banks (always used)
#   Step B — Try to read FDIC bulk CSV (call_report_2022Q4.csv) for KBW banks
#             Download from: https://banks.data.fdic.gov/api/financials?
#               filters=REPDTE%3A20221231
#               &fields=CERT,ASSET,DEP,UNINSDEP,LNLSGR,TIER1LEV
#               &limit=10000&output=csv
#             Save as call_report_2022Q4.csv in your working directory.
#   Step B fallback — if CSV not found, use hard-coded KBW values.
#
# FDIC bulk CSV amounts are in $thousands:
#   assets_bn  = ASSET   / 1,000,000   (thousands → billions)
#   ltd_ratio  = LNLSGR  / DEP * 100
#   unins_pct  = UNINSDEP / DEP * 100
#   tier1_lev  = TIER1LEV (already in %)
 
# ── Step A: Core 14 banks (hard-coded, always reliable) ──────
bank_chars_core <- tribble(
  ~ticker, ~uninsured_dep_pct, ~assets_bn, ~tier1_lev, ~ltd_ratio,
  "SIVB",  94.3,  209.0, 7.96,  42.0,
  "SBNY",  89.3,  110.4, 8.79,  83.9,
  "FRC",   67.3,  212.6, 8.51,  94.1,
  "ZION",  49.2,   89.5, 7.65,  76.5,
  "WAL",   52.6,   67.7, 8.22,  98.7,
  "PACW",  71.1,   44.3, 9.70,  84.4,
  "CMA",   59.3,   85.5, 9.01,  71.9,
  "KEY",   48.2,  187.6, 8.78,  81.8,
  "RF",    34.8,  154.2, 8.80,  71.7,
  "JPM",   59.5, 3201.9, 8.30,  46.1,
  "BAC",   41.7, 2418.5, 7.68,  50.4,
  "WFC",   39.3, 1717.5, 8.34,  63.8,
  "USB",   52.2,  585.1, 8.05,  71.2,
  "TFC",   41.8,  546.0, 8.54,  76.0
)
 
# ── FDIC CERT lookup table (verified CERT numbers) ───────────
cert_map <- tribble(
  ~ticker, ~CERT,
  "FITB",   6672,   # Fifth Third Bank
  "HBAN",   6560,   # Huntington National Bank
  "CFG",   57957,   # Citizens Bank NA
  "FHN",    4977,   # First Horizon Bank
  "COLB",  17266,   # Umpqua Bank (Columbia)
  "OZK",     110,   # Bank OZK
  "WTFC",   2289,   # Wintrust Bank
  "BOKF",   4091,   # Bank of Oklahoma
  "UMBF",  10167,   # UMB Bank
  "VLY",    9396,   # Valley National Bank
  "WBS",   18221,   # Webster Bank
  "HWC",   12441,   # Hancock Whitney Bank
  "EWBC",  31628,   # East West Bank
  "CBSH",  24998,   # Commerce Bank
  "FFBC",   6600,   # First Financial Bank
  "PB",     1971,   # Prosperity Bank
  "TCBI",  34383,   # Texas Capital Bank
  "ONB",    3832,   # Old National Bank
  "UBSI",  22858,   # United Bank
  "GBCI",  30788,   # Glacier Bank
  "INDB",   9712,   # Rockland Trust
  "WSFS",    588,   # WSFS Bank
  "BANR",  28489,   # Banner Bank
  "FFIN",  30618,   # First Financial Bank Texas
  "TRMK",   1039,   # Trustmark National Bank
  "SFNC",  15019,   # First Bank (Southern First)
  "HAFC",  24170,   # Hanmi Bank
  "CVBF",  21966,   # Citizens Business Bank
  "RNST",  12437,   # Renasant Bank
  "WAFD",  28088,   # Washington Federal Bank
  "CATY",  30274,   # Cathay Bank
  "EBC",     902,   # Eastern Bank
  "GABC",  27314,   # German American Bank
  "IBCP",  23007    # Independent Bank
)
 
# ── Step B: Read pre-built KBW characteristics CSV ───────────
# kbw_bank_chars.csv was built from financials.json (FDIC bulk download)
# combined with hard-coded fallbacks where the JSON had wrong subsidiaries.
# 19/34 banks use real FDIC asset/LTD figures; all use verified uninsured
# deposit % and Tier 1 leverage from FDIC Call Report annual disclosures.
bank_chars_kbw <- NULL
 
kbw_path <- c("kbw_bank_chars.csv", file.path("data", "kbw_bank_chars.csv"))
kbw_path <- kbw_path[file.exists(kbw_path)][1]
if (!is.na(kbw_path)) {
  bank_chars_kbw <- read_csv(kbw_path, show_col_types = FALSE) %>%
    filter(ticker %in% available) %>%
    select(ticker, assets_bn, uninsured_dep_pct, tier1_lev, ltd_ratio)
  message("  OK ", kbw_path, ": ", nrow(bank_chars_kbw), " KBW banks loaded")
} else {
  message("  kbw_bank_chars.csv not found — using hard-coded fallback")
}
 
# ── Combine core + KBW, print summary ────────────────────────
# FIX (duplicated banks -- this one changed N). bind_rows() stacked the 14-bank
# hand-checked core table on top of the 46-bank KBW CSV without deduplicating,
# and 12 tickers appear in BOTH (every core bank except PACW and CMA, which the
# CSV does not carry). The result was 60 rows for 48 banks, so the cross-sectional
# regression in R/08 ran with SIVB, SBNY, FRC, ZION, WAL, KEY, RF, JPM, BAC, WFC,
# USB and TFC each counted TWICE -- exactly the banks that drive the result.
# Verified live: N was 60 before this fix and 48 after. Table 5 reports N = 48, so
# the thesis's numbers cannot have come from this path; 12_verify_n48.R applies
# distinct() and is presumably where they came from.
# distinct() keeps the FIRST match, i.e. the hand-checked core values, which is
# the intended precedence (Section 4.3 sources PACW and CMA that way).
bank_chars <- bind_rows(bank_chars_core, bank_chars_kbw) %>%
  distinct(ticker, .keep_all = TRUE) %>%
  filter(ticker %in% available)
 
message("  Bank characteristics: ", nrow(bank_chars),
        " banks ready for regression\n")
print(bank_chars %>% select(ticker, assets_bn, uninsured_dep_pct,
                             tier1_lev, ltd_ratio))
 
# ── UPGRADE 5: Regulatory regime dummy ───────────────────────────────────────
# 2018 Economic Growth, Regulatory Relief, and Consumer Protection Act (EGRRCPA)
# Raised the SIFI threshold from $50B to $250B.
# Banks < $100B: fully exempt from enhanced prudential standards & Basel III LCR
# Banks $100B–$250B: subject to stress testing but exempt from strictest LCR
# Banks > $250B (Category I/II) or G-SIBs: full Basel III + TLAC requirements
bank_chars <- bank_chars %>%
  mutate(
    reg_exempt = case_when(
      assets_bn < 100  ~ 1L,   # fully exempt from EGRRCPA enhanced standards
      assets_bn < 250  ~ 0L,   # partial (stress-test only)
      TRUE             ~ 0L    # full regulation
    ),
    gsib = if_else(assets_bn > 700, 1L, 0L),  # G-SIBs: JPM, BAC, WFC
    size_cat = case_when(
      assets_bn < 100  ~ "Small Regional",
      assets_bn < 500  ~ "Mid-Size Regional",
      assets_bn < 700  ~ "Large Regional",
      TRUE             ~ "G-SIB / SIFI"
    )
  )
 
cat("\n=== BANK CHARACTERISTICS (with regulatory dummies) ===\n")
print(bank_chars %>% select(ticker, assets_bn, uninsured_dep_pct,
                             tier1_lev, ltd_ratio, reg_exempt, gsib))
