# =====================================================================
# 03_returns_dataframe.R
# Build the merged daily log-returns data frame.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 3. BUILD RETURNS DATA FRAME ──────────────────────────────
# FIX 19: difference each ticker on ITS OWN observed prices, then merge.
# The old code merged the price levels first and differenced the aligned
# matrix, so a bank that was halted and later resumed lost the return across
# the halt. First Republic was suspended on 1 May 2023 and resumed over the
# counter on 3 May at $0.33 from $3.51, and that -234% abnormal return -- the
# largest single observation in the study, and the whole content of Event 5 --
# was silently dropped. It took the Event 5 CAAR from -14.35% to -9.26% and
# one row out of the attention panel (697 -> 696). Differencing per ticker
# makes the resumption-day return span the halt, which is the correct
# treatment and the one Section 3.2 of the thesis describes.
log_ret <- do.call(merge,
  lapply(available, function(t) {
    px <- Ad(get(t))
    px <- px[!is.na(px)]          # <- the fix: drop gaps BEFORE differencing
    r  <- diff(log(px))
    colnames(r) <- t
    r
  })
)
 
market_xts <- Ad(GSPC); colnames(market_xts) <- "Market"
market_ret <- diff(log(market_xts))
 
# Only drop rows where S&P500 is NA — NOT where individual stocks are NA
# (na.omit on full matrix would collapse Events 3-5 onto Event 2's date
#  because delisted banks like SIVB have no data after March 9)
all_ret <- merge(log_ret, market_ret)
all_ret <- all_ret[!is.na(coredata(all_ret)[, "Market"]), ]
 
returns_df <- data.frame(Date = index(all_ret),
                          coredata(all_ret)) %>%
  mutate(Date = as.Date(Date))
 
cat("Returns data: ", nrow(returns_df), "trading days,",
    ncol(returns_df) - 1, "series\n\n")
