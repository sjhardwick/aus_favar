# update_cache.R
# Run this script to download fresh data and save to cache.
# Usage: Rscript update_cache.R

library(dplyr)
library(tidyr)
library(lubridate)
library(readabs)
library(readrba)

source("R/data_pull.R")
source("R/cache.R")

message("Downloading ABS target series...")
abs_df <- pull_abs_data()

message("Downloading RBA cash rate...")
rba_df <- pull_rba_data()

message("Downloading panel data...")
panel_df <- pull_panel_data()

raw_data <- list(
  targets = bind_rows(abs_df, rba_df),
  panel   = panel_df
)

cache_save(raw_data)
message("Cache saved to ", CACHE_PATH, " at ", Sys.time())
message("Target series: ", paste(unique(raw_data$targets$series), collapse = ", "))
message("Panel series (", length(unique(raw_data$panel$series)), "): ",
        paste(unique(raw_data$panel$series), collapse = ", "))
