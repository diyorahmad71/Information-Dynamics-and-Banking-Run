# =====================================================================
# 07_google_trends.R
# Google Trends loader (CSV first, API fallback).
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 7. GOOGLE TRENDS ─────────────────────────────────────────
# Priority: (1) load from downloaded CSV files if present,
#           (2) fall back to gtrendsR API pull,
#           (3) if both fail, set info_index = NA and continue.
#
# CSV files: downloaded from trends.google.com for BOTH keywords in a single
# query (so they share one scale), worldwide, date range Jan 1 - Jun 30 2023,
# daily frequency. The old comment here said "US region, weekly frequency",
# which matched neither the bundled files nor Sections 3.6 and 4.4.
# Expected files in working directory:
#   google_trends_svb.csv     — contains "svb" column
#   google_trends_bankrun.csv — contains "Bank run" column
#
# Actual files used: provided by author from Google Trends download.
message("\nLoading Google Trends data...")
 
# FIX (keywords). The thesis (Sections 3.6 and 4.4) builds the composite from
# exactly two terms, and the bundled CSVs contain those two. This vector listed
# four, so if the API fallback ever fired it would have built a FOUR-keyword
# index while the text described a two-keyword one.
gt_keywords <- c("SVB", "bank run")
gt_long     <- NULL
info_df     <- NULL
returns_df$info_index <- NA_real_
 
# ── 7a. LOAD FROM CSV FILES (primary source) ──────────────────
# These CSV files were downloaded directly from trends.google.com
# and contain actual search interest data for the crisis period.
# FIX (paths). These looked only in data/, but the repository ships the two
# CSVs in the project ROOT and has no data/ directory, so on a fresh clone the
# files were never found and the loader fell through to the gtrendsR API --
# which needs a live connection and returns a differently-scaled series.
# Now checks ./ and data/, exactly as load_csv() in R/02 already does.
find_data <- function(fname) {
  cand <- c(fname, file.path("data", fname))
  hit  <- cand[file.exists(cand)]
  if (length(hit)) hit[1] else cand[1]
}
csv_svb_path     <- find_data("google_trends_svb.csv")
csv_bankrun_path <- find_data("google_trends_bankrun.csv")
 
csv_loaded <- FALSE
tryCatch({
  if (file.exists(csv_svb_path) && file.exists(csv_bankrun_path)) {
    message("  Loading Google Trends from CSV files...")
 
    # File 1: SVB search interest
    raw_svb <- read_csv(csv_svb_path, skip = 0, show_col_types = FALSE)
    colnames(raw_svb) <- tolower(colnames(raw_svb))
    svb_df <- raw_svb %>%
      select(time, svb) %>%
      rename(Date = time, hits = svb) %>%
      mutate(Date = as.Date(Date), hits = as.numeric(hits),
             keyword = "SVB",
             # FIX (shared scale). The two CSVs come from ONE Google Trends
             # query, so they already share a single scale: 'SVB' peaks at 100
             # and 'bank run' at 7, meaning 'bank run' volume is about 7% of
             # 'SVB' volume at the peak. Re-normalising each series to its own
             # maximum -- which this line used to do -- throws that away and
             # silently inflates 'bank run' roughly fourteen-fold, giving the
             # rarer term equal weight in the composite. Section 4.4 of the
             # thesis describes the shared-scale version (unscaled average of
             # 53.5 at the peak, ~2.5 on 1 May, ~40x below peak); the previous
             # code produced 100, 3.35 and ~30x instead. Keep the series as
             # downloaded and rescale ONLY the composite, below.
             hits_scaled = hits)
 
    # File 2: Bank run search interest
    raw_br <- read_csv(csv_bankrun_path, skip = 0, show_col_types = FALSE)
    colnames(raw_br) <- tolower(colnames(raw_br))
    # Find the "bank run" column (may be named "bank run" or similar)
    br_col <- colnames(raw_br)[grepl("bank", colnames(raw_br), ignore.case = TRUE)][1]
    bankrun_df <- raw_br %>%
      select(time, all_of(br_col)) %>%
      rename(Date = time, hits = all_of(br_col)) %>%
      mutate(Date = as.Date(Date), hits = as.numeric(hits),
             keyword = "bank run",
             hits_scaled = hits)   # FIX: keep the shared Trends scale (see above)
 
    # Combine into gt_long
    gt_long <<- bind_rows(svb_df, bankrun_df) %>%
      filter(!is.na(hits))
 
    # Build the composite information index: equally weighted average of the
    # two series AS DOWNLOADED (they share one Trends scale), then rescale so
    # the composite's own peak equals 100. This is what Section 4.4 describes.
    info_df <<- gt_long %>%
      group_by(Date) %>%
      summarise(info_index = mean(hits_scaled, na.rm = TRUE), .groups = "drop") %>%
      mutate(info_index = info_index / max(info_index, na.rm = TRUE) * 100)

    # Join to returns_df. The bundled extracts are DAILY over Jan-Jun 2023, so
    # the fill below is a no-op guard for any gap (e.g. a market holiday that
    # Trends does report); it is not a weekly-to-daily expansion.
    # FIX (column collision -- this one silently disabled the whole CSV path).
    # Line 24 above pre-creates returns_df$info_index as NA. Joining a second
    # info_index on top produced info_index.x / info_index.y, so the fill()
    # below then failed with "Column `info_index` doesn't exist" -- caught by
    # the enclosing tryCatch, which reported an EMPTY message and fell through
    # to the API. Net effect: the bundled CSVs were never used. Drop the
    # placeholder before joining.
    returns_df <<- returns_df %>%
      select(-any_of("info_index")) %>%
      left_join(info_df, by = "Date") %>%
      arrange(Date) %>%
      fill(info_index, .direction = "down")

    message("  ✓ Google Trends loaded from CSV: SVB + Bank Run (",
            nrow(gt_long), " daily observations across both keywords)")
    csv_loaded <<- TRUE
 
  } else {
    message("  CSV files not found — will try gtrendsR API instead.")
  }
}, error = function(e) {
  message("  ! CSV load error: ", e$message, " — falling back to API.")
})
 
# ── 7b. API FALLBACK (if CSV not available) ───────────────────
if (!csv_loaded) {
  message("  Trying gtrendsR API...")
  tryCatch({
    # FIX (one query, worldwide). Two changes here. (1) Both keywords now go in
    # a SINGLE gtrends call, so Google returns them on one shared scale, exactly
    # as the bundled CSVs are. Querying them separately -- as this did -- gives
    # each series its own 0-100 scale and hands the rarer term equal weight in
    # the composite. (2) geo is now "" (worldwide) rather than "US", because
    # Sections 3.6 and 4.4 describe a worldwide series and treat that as one of
    # the design's stated limitations.
    trends_list <- list(tryCatch({
      Sys.sleep(1)
      gtrends(keyword = gt_keywords, geo = "",
              time = "2023-01-01 2023-06-30",
              onlyInterest = TRUE)$interest_over_time %>%
        mutate(hits = as.character(hits))       # force consistent type
    }, error = function(e) { message("  x Trends pull failed: ", e$message); NULL }))
 
  trends_ok <- Filter(Negate(is.null), trends_list)
  if (length(trends_ok) == 0) stop("All keyword pulls failed")
 
  gt_long <<- bind_rows(trends_ok) %>%
    as_tibble() %>%
    mutate(Date = as.Date(date),
           hits = ifelse(hits == "<1", "0", hits),
           hits = as.numeric(hits)) %>%
    select(Date, keyword, hits) %>%
    group_by(Date, keyword) %>%
    summarise(hits = mean(hits, na.rm = TRUE), .groups = "drop") %>%
    # FIX: one query means one shared scale, so do NOT re-normalise per keyword
    # (see the CSV branch above for why that matters).
    mutate(hits_scaled = hits) %>%
    ungroup()
 
  info_df <<- gt_long %>%
    group_by(Date) %>%
    summarise(info_index = mean(hits_scaled, na.rm = TRUE), .groups = "drop") %>%
    mutate(info_index = info_index / max(info_index, na.rm = TRUE) * 100)
 
  returns_df <<- returns_df %>%
    select(-any_of("info_index")) %>%          # FIX: see the CSV branch above
    left_join(info_df, by = "Date") %>%
    arrange(Date) %>%
    fill(info_index, .direction = "down")
 
    message("  OK Google Trends loaded via API (", length(trends_ok), "/",
            length(gt_keywords), " keywords)")
 
  }, error = function(e) {
    message("  ! Google Trends API skipped: ", e$message)
    message("  Core results (CARs, regressions, plots) are unaffected.")
  })
} # end if (!csv_loaded)
 
# ── 7c. DUAL-PANEL CHART: Google Trends + CAAR per Event ─────
# Figure 1 in thesis: information dynamics visualisation.
# Top panel  — Google Trends search interest (SVB + bank run).
# Bottom panel — absolute CAAR per event across 46 banks.
# Both share the same time axis; peaks align at Event 2 (Mar 10-12).
if (!is.null(gt_long) && nrow(gt_long) > 0) {
  tryCatch({
    # CAAR summary per event
    caar_summary <- results_summary %>%
      group_by(event_id) %>%
      summarise(CAAR = mean(CAR, na.rm = TRUE), .groups = "drop") %>%
      left_join(EVENTS %>% select(id, date), by = c("event_id" = "id")) %>%
      mutate(CAAR_abs = abs(CAAR) * 100,
             label    = paste0(round(CAAR * 100, 2), "%"))
 
    # Top panel: Google Trends
    p_gt_top <- ggplot(gt_long, aes(x = Date, y = hits_scaled, colour = keyword)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.8) +
      geom_vline(data = EVENTS, aes(xintercept = date),
                 linetype = "dashed", colour = "grey40", linewidth = 0.6) +
      annotate("text", x = as.Date("2023-03-12"), y = 105,
               label = "E2: SVB+SBNY\npeak", size = 2.8, colour = "#c0392b",
               fontface = "bold", hjust = 0.5) +
      scale_colour_manual(
        values = c("SVB" = "#c0392b", "bank run" = "#1a7a6e"),
        name = "Search term") +
      scale_y_continuous(limits = c(0, 115), labels = function(x) paste0(x)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      labs(y = "Search Interest (0–100)", x = NULL,
           title = "Information Dynamics: Google Search Interest and Abnormal Bank Returns",
           subtitle = "Top: Weekly Google Trends (0–100). Source: Google Trends (trends.google.com).") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top",
            plot.title    = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(size = 9, colour = "grey40"))
 
    # Bottom panel: CAAR bars
    event_colors <- c("#c0392b","#c0392b","#e67e22","#8e44ad","#2980b9")
    p_gt_bot <- ggplot(caar_summary, aes(x = date, y = CAAR_abs, fill = factor(event_id))) +
      geom_col(width = 12, show.legend = FALSE) +
      geom_text(aes(label = paste0(label, "\n(E", event_id, ")")),
                vjust = -0.3, size = 2.8, fontface = "bold") +
      scale_fill_manual(values = setNames(event_colors, 1:5)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                   limits = range(gt_long$Date)) +
      scale_y_continuous(limits = c(0, 26),
                         labels = function(x) paste0(x, "%")) +
      labs(y = "|CAAR| (%)", x = "Week (2023)",
           subtitle = paste0("Bottom: Absolute CAAR per event across ",
                             length(unique(results_summary$ticker)),
                             " banks. Source: Yahoo Finance / author's calculations.")) +
      theme_minimal(base_size = 11) +
      theme(plot.subtitle = element_text(size = 9, colour = "grey40"))
 
    # Combine with patchwork (install if missing)
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      install.packages("patchwork")
    }
    library(patchwork)
    p_dual <- p_gt_top / p_gt_bot + plot_layout(heights = c(2, 1))
    ggsave("output/google_trends_dual_panel.png", p_dual,
           width = 11, height = 7, dpi = 300)
    message("  ✓ google_trends_dual_panel.png saved (Figure 1 in thesis)")
 
  }, error = function(e) {
    message("  ! Dual-panel chart skipped: ", e$message)
  })
}
