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
  # FIX: name a CRAN mirror explicitly. Without repos=, a non-interactive run
  # (Rscript, CI, headless server) fails with "trying to use CRAN without
  # setting a mirror" instead of installing. R/11 already passed repos=; this
  # brings the main installer into line.
  install.packages(new_pkgs, dependencies = TRUE,
                   repos = getOption("repos_thesis", "https://cloud.r-project.org"))
}
invisible(lapply(pkgs, library, character.only = TRUE))
message("✓ All packages loaded\n")
