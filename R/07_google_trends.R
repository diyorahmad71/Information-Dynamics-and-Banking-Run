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
# CSV files: download from trends.google.com for each keyword,
# date range Jan 1 – Jun 30 2023, US region, weekly frequency.
# Expected files in working directory:
#   google_trends_svb.csv     — contains "svb" column
#   google_trends_bankrun.csv — contains "Bank run" column
#
# Actual files used: provided by author from Google Trends download.
message("\nLoading Google Trends data...")
 
gt_keywords <- c("SVB", "bank run", "FDIC insurance", "Silicon Valley Bank")
gt_long     <- NULL
info_df     <- NULL
returns_df$info_index <- NA_real_
 
# ── 7a. LOAD FROM CSV FILES (primary source) ──────────────────
# These CSV files were downloaded directly from trends.google.com
# and contain actual search interest data for the crisis period.
csv_svb_path     <- "data/google_trends_svb.csv"
csv_bankrun_path <- "data/google_trends_bankrun.csv"
 
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
             hits_scaled = hits / max(hits, na.rm = TRUE) * 100)
 
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
             hits_scaled = hits / max(hits, na.rm = TRUE) * 100)
 
    # Combine into gt_long
    gt_long <<- bind_rows(svb_df, bankrun_df) %>%
      filter(!is.na(hits))
 
    # Build composite info_index (average of both series per week)
    info_df <<- gt_long %>%
      group_by(Date) %>%
      summarise(info_index = mean(hits_scaled, na.rm = TRUE), .groups = "drop") %>%
      mutate(info_index = info_index / max(info_index, na.rm = TRUE) * 100)
 
    # Join to returns_df (weekly → fill forward to match daily returns)
    returns_df <<- returns_df %>%
      left_join(info_df, by = "Date") %>%
      arrange(Date) %>%
      fill(info_index, .direction = "down")
 
    message("  ✓ Google Trends loaded from CSV: SVB + Bank Run (",
            nrow(gt_long), " weekly observations)")
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
    trends_list <- map(gt_keywords, function(kw) {
      Sys.sleep(1)
      res <- tryCatch(
        gtrends(keyword = kw, geo = "US",
                time = "2023-01-01 2023-06-30",
                onlyInterest = TRUE)$interest_over_time,
        error = function(e) { message("  x '", kw, "': ", e$message); NULL }
      )
    if (is.null(res)) return(NULL)
    res %>% mutate(hits = as.character(hits))  # force consistent type
  })
 
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
    group_by(keyword) %>%
    mutate(hits_scaled = hits / max(hits, na.rm = TRUE) * 100) %>%
    ungroup()
 
  info_df <<- gt_long %>%
    group_by(Date) %>%
    summarise(info_index = mean(hits_scaled, na.rm = TRUE), .groups = "drop") %>%
    mutate(info_index = info_index / max(info_index, na.rm = TRUE) * 100)
 
  returns_df <<- returns_df %>%
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
    ggsave("google_trends_dual_panel.png", p_dual,
           width = 11, height = 7, dpi = 300)
    message("  ✓ google_trends_dual_panel.png saved (Figure 1 in thesis)")
 
  }, error = function(e) {
    message("  ! Dual-panel chart skipped: ", e$message)
  })
}
