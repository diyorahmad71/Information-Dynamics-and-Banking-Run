# =====================================================================
# 09_plots.R
# Figures, including Figure 1 (Google Trends / CAAR dual panel).
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 9. PLOTS ─────────────────────────────────────────────────
# Plot 1 — CAAR over event window, all 5 events
p1 <- AAR_table %>%
  left_join(EVENTS %>% select(id, name), by = c("event_id" = "id")) %>%
  ggplot(aes(x = day, y = CAAR, colour = name, group = name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = 0, linetype = "dotted",
             colour = "red", linewidth = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1, scale = 1)) +
  scale_colour_brewer(palette = "Set1") +
  labs(
    title    = "Cumulative Average Abnormal Returns (CAAR)",
    subtitle = paste0("Event window [-2, +2] | 2023 Banking Crisis | N=",
                      length(available), " banks"),
    x = "Trading Days Relative to Event", y = "CAAR (%)", colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))
print(p1)
ggsave("CAAR_plot.png", p1, width = 10, height = 6, dpi = 300)
message("  ✓ CAAR_plot.png saved")
 
# Plot 2 — CAR by bank at Event 2
p2 <- results_summary %>%
  filter(event_id == 2) %>%
  left_join(bank_chars %>% select(ticker, size_cat), by = "ticker") %>%
  mutate(ticker = fct_reorder(ticker, CAR),
         group  = coalesce(size_cat,
           case_when(ticker %in% c("SIVB","SBNY","FRC") ~ "Failed",
                     TRUE ~ "Regional"))) %>%
  ggplot(aes(x = ticker, y = CAR * 100, fill = group)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(title    = "Cumulative Abnormal Returns — Event 2",
       subtitle = "SVB & Signature Bank Closure (March 10, 2023)",
       x = NULL, y = "CAR (%)", fill = "Bank Group") +
  theme_minimal(base_size = 11)
print(p2)
ggsave("CAR_by_bank_E2.png", p2, width = 9, height = 8, dpi = 300)
message("  ✓ CAR_by_bank_E2.png saved")
 
# Plot 3 — Uninsured deposits vs CAR scatter
p3 <- reg_df %>%
  ggplot(aes(x = uninsured_dep_pct, y = CAR * 100)) +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#e74c3c", fill = "#fadbd8", linewidth = 0.8) +
  geom_point(aes(size = assets_bn), colour = "#2c3e50", alpha = 0.8) +
  geom_text_repel(aes(label = ticker), size = 3, max.overlaps = 20) +
  scale_size_continuous(name = "Total Assets ($B)", range = c(2, 8)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Uninsured Deposits vs. CAR at SVB Closure",
    subtitle = paste0("N = ", nrow(reg_df), " banks | Point size = total assets"),
    x = "Uninsured Deposits (% of Total, Q4 2022)",
    y = "CAR [-2,+2] (%)"
  ) +
  theme_minimal(base_size = 13)
print(p3)
ggsave("uninsured_vs_CAR.png", p3, width = 9, height = 6, dpi = 300)
message("  ✓ uninsured_vs_CAR.png saved")
 
# Plot 4 — Calendar-time portfolio
p_ct <- ct_portfolio %>%
  ggplot(aes(x = day, y = port_CAR * 100)) +
  geom_col(aes(y = port_AR * 100, fill = port_AR > 0),
           alpha = 0.6, show.legend = FALSE) +
  geom_line(colour = "#2c3e50", linewidth = 1.2) +
  geom_point(colour = "#2c3e50", size = 3) +
  geom_text(aes(label = paste0(round(port_CAR * 100, 1), "%"),
                y = port_CAR * 100 + 0.5), size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#c0392b")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Calendar-Time Portfolio: Cumulative Abnormal Returns",
    subtitle = "Clustering-robust test (MacKinlay 1997) | Event 2, March 10 2023",
    x = "Trading Days Relative to Event", y = "CAR / Cumulative AR (%)"
  ) +
  theme_minimal(base_size = 13)
print(p_ct)
ggsave("calendar_time_portfolio.png", p_ct, width = 9, height = 5, dpi = 300)
message("  ✓ calendar_time_portfolio.png saved")
 
# Plot 5 — Google Trends (daily if available)
if (!is.null(gt_long) && !all(is.na(returns_df$info_index))) {
  kw_colors <- c("SVB"                 = "#e74c3c",
                 "Silicon Valley Bank" = "#2980b9",
                 "bank run"            = "#27ae60",
                 "FDIC insurance"      = "#8e44ad")
 
  p5 <- ggplot() +
    geom_line(data = gt_long,
              aes(x = Date, y = hits_scaled, colour = keyword),
              linewidth = 0.7, alpha = 0.8) +
    geom_line(data = info_df %>% filter(!is.na(info_index)),
              aes(x = Date, y = info_index),
              colour = "black", linewidth = 1.3) +
    geom_vline(data = EVENTS,
               aes(xintercept = date, linetype = name),
               colour = "grey30", linewidth = 0.6) +
    scale_colour_manual(values = kw_colors, name = "Search term") +
    scale_linetype_manual(values = rep("dashed", 5), name = "Crisis event") +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      title    = "Information Dynamics: Google Search Interest (2023)",
      subtitle = "Scaled 0-100 per keyword | Black line = composite index",
      x = NULL, y = "Search Interest (0-100)"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.text = element_text(size = 8))
  print(p5)
  ggsave("google_trends_info.png", p5, width = 11, height = 6, dpi = 300)
  message("  ✓ google_trends_info.png saved")
}
