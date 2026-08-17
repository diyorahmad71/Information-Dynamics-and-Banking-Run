# =====================================================================
# 00_packages.R
# Install (once) and load required libraries.
# Sourced in sequence by run_all.R (shared global environment).
# =====================================================================

# ── 0. PACKAGES ─────────────────────────────────────────────
pkgs <- c("quantmod", "tidyverse", "xts", "sandwich",
          "lmtest", "ggrepel", "scales", "gtrendsR",
          "jsonlite", "fixest")
 
new_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new_pkgs)) {
  message("Installing missing packages: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, dependencies = TRUE)
}
invisible(lapply(pkgs, library, character.only = TRUE))
message("✓ All packages loaded\n")
