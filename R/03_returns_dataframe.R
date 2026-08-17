# =====================================================================
# 03_returns_dataframe.R
# Build the merged daily log-returns data frame.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 3. BUILD RETURNS DATA FRAME ──────────────────────────────
prices_xts <- do.call(merge,
  lapply(available, function(t) {
    px <- Ad(get(t))
    colnames(px) <- t
    px
  })
)
 
market_xts <- Ad(GSPC); colnames(market_xts) <- "Market"
log_ret    <- diff(log(prices_xts))
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
