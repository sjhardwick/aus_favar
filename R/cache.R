# cache.R
# Functions for caching downloaded data to avoid slow startup

CACHE_PATH <- "data/raw_data.rds"

#' Save raw data to cache
#'
#' @param raw_data List with targets and panel data frames
cache_save <- function(raw_data) {
  if (!dir.exists("data")) dir.create("data")
  raw_data$cached_at <- Sys.time()
  saveRDS(raw_data, CACHE_PATH)
}

#' Load raw data from cache
#'
#' @return List with targets and panel data frames, or NULL if no cache
cache_load <- function() {
  if (!file.exists(CACHE_PATH)) return(NULL)
  readRDS(CACHE_PATH)
}

#' Check whether a cache file exists
#'
#' @return TRUE if cache exists
cache_exists <- function() {
  file.exists(CACHE_PATH)
}

#' Return a human-readable string describing the cache age
#'
#' @return Character string, e.g. "2 days ago"
cache_age_label <- function() {
  if (!cache_exists()) return("No cached data")
  cached <- readRDS(CACHE_PATH)
  if (is.null(cached$cached_at)) return("Unknown age")
  age <- difftime(Sys.time(), cached$cached_at, units = "hours")
  if (age < 1) {
    "Less than 1 hour ago"
  } else if (age < 24) {
    paste0(round(age, 0), " hours ago")
  } else {
    paste0(round(as.numeric(age) / 24, 0), " days ago")
  }
}
