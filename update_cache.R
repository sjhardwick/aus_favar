# update_cache.R
# Run this script to download fresh data and save to cache.
# Usage: Rscript --vanilla update_cache.R

if (!"renv" %in% loadedNamespaces()) {
  source("renv/activate.R")
}

library(dplyr)
library(tidyr)
library(lubridate)
library(readabs)
library(readrba)

source("R/data_pull.R")
source("R/cache.R")

log_step <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", ...)
}

log_step("Downloading ABS target series...")
abs_df <- pull_abs_data()

log_step("Downloading RBA cash rate...")
rba_df <- pull_rba_data()

log_step("Downloading panel data...")
panel_df <- pull_panel_data()

raw_data <- list(
  targets = bind_rows(abs_df, rba_df),
  panel   = panel_df
)

cache_save(raw_data)
log_step("Cache saved to ", CACHE_PATH)
log_step("Target series: ", paste(unique(raw_data$targets$series), collapse = ", "))
log_step("Panel series (", length(unique(raw_data$panel$series)), "): ",
         paste(unique(raw_data$panel$series), collapse = ", "))

# Force a clean non-interactive exit in case package-level background state
# keeps Rscript alive after the final expression.
quit(save = "no", status = 0)
