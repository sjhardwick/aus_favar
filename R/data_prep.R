# data_prep.R
# Functions for data cleaning, alignment, and stationarity transforms

# Transformation codes:
#   1 = no transformation (levels, e.g. rates)
#   2 = log-difference (for positive levels like GDP, CPI)
#   3 = first difference (for series that can be negative)
#   4 = second difference

# Default transformation map for known series
TRANSFORM_MAP <- list(
  gdp                 = 2L,
  cpi                 = 2L,
  unemployment_rate   = 1L,
  cash_rate           = 1L,
  employment          = 2L,
  participation_rate  = 1L,
  hours_worked        = 2L,
  fulltime_employment = 2L,
  parttime_employment = 2L,
  wpi                 = 2L,
  retail_turnover     = 2L,
  building_approvals  = 2L,
  consumption         = 2L,
  investment          = 2L,
  exports             = 2L,
  imports             = 2L,
  bill_rate_90d       = 1L,
  twi                 = 2L,
  aud_usd             = 2L,
  m3                  = 2L,
  broad_money         = 2L,
  total_credit        = 2L,
  housing_credit      = 2L,
  trade_balance       = 3L,
  ppi                 = 2L,
  commodity_prices_aud     = 2L,
  bulk_commodity_prices    = 2L,
  rural_commodity_prices   = 2L,
  base_metals_prices       = 2L,
  nonrural_commodity_prices = 2L,
  coal_exports             = 2L,
  metal_ores_exports       = 2L,
  resource_exports         = 2L,
  terms_of_trade           = 2L
)

#' Align series to quarterly frequency
#'
#' Aggregates monthly or daily series to quarterly by taking the end-of-quarter
#' value for stock/rate variables and the mean for flow variables.
#' @param df Tidy data frame with columns: date, series, value
#' @return Data frame aggregated to quarterly frequency with date set to
#'   quarter-end
align_quarterly <- function(df) {
  # Rate/stock series: take end-of-quarter value
  rate_series <- c("unemployment_rate", "cash_rate", "participation_rate",
                   "bill_rate_90d", "twi", "aud_usd")

  df <- df |>
    dplyr::mutate(
      quarter = lubridate::quarter(date),
      year    = lubridate::year(date),
      agg_type = dplyr::if_else(series %in% rate_series, "last", "mean")
    )

  # Split, aggregate separately, recombine
  rate_df <- df |>
    dplyr::filter(agg_type == "last") |>
    dplyr::group_by(series, year, quarter) |>
    dplyr::summarise(value = dplyr::last(value, order_by = date),
                     .groups = "drop")

  flow_df <- df |>
    dplyr::filter(agg_type == "mean") |>
    dplyr::group_by(series, year, quarter) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE),
                     .groups = "drop")

  dplyr::bind_rows(rate_df, flow_df) |>
    dplyr::mutate(
      month_num = quarter * 3L,
      date = as.Date(paste0(year, "-",
                             formatC(month_num, width = 2, flag = "0"),
                             "-01")),
      date = date + lubridate::days_in_month(date) - 1L
    ) |>
    dplyr::select(date, series, value)
}

#' Apply stationarity transformations
#'
#' Uses TRANSFORM_MAP to decide how to transform each series. Unknown series
#' default to log-difference if all values are positive, otherwise first diff.
#' @param df Tidy quarterly data frame with columns: date, series, value
#' @return List with:
#'   - data: transformed tidy data frame
#'   - transforms: named list of transformation codes applied
transform_stationary <- function(df) {
  series_names <- unique(df$series)
  transforms_used <- list()
  out_list <- list()

  for (s in series_names) {
    sdf <- df |>
      dplyr::filter(series == s) |>
      dplyr::arrange(date)

    code <- TRANSFORM_MAP[[s]]
    if (is.null(code)) {
      code <- if (all(sdf$value > 0, na.rm = TRUE)) 2L else 3L
    }
    transforms_used[[s]] <- code

    vals <- sdf$value
    # Guard: if log-diff requested but values contain non-positive, fall back
    if (code == 2L && any(vals <= 0, na.rm = TRUE)) {
      code <- 3L
      transforms_used[[s]] <- code
    }
    transformed <- switch(
      as.character(code),
      "1" = vals,
      "2" = c(NA_real_, diff(log(vals))) * 100,
      "3" = c(NA_real_, diff(vals)),
      "4" = c(NA_real_, NA_real_, diff(diff(vals)))
    )

    sdf$value <- transformed
    out_list[[s]] <- sdf
  }

  list(
    data       = dplyr::bind_rows(out_list),
    transforms = transforms_used
  )
}

#' Back-transform forecasts to original scale
#'
#' Given a forecast in transformed space, convert back to levels.
#' @param forecast_vals Numeric vector of forecast values (transformed)
#' @param last_level Last observed level (original scale)
#' @param code Transformation code (1, 2, 3, or 4)
#' @return Numeric vector of forecasted levels
back_transform <- function(forecast_vals, last_level, code) {
  h <- length(forecast_vals)
  levels <- numeric(h)

  switch(
    as.character(code),
    "1" = {
      levels <- forecast_vals
    },
    "2" = {
      levels[1] <- last_level * exp(forecast_vals[1] / 100)
      if (h > 1) {
        for (i in 2:h) {
          levels[i] <- levels[i - 1] * exp(forecast_vals[i] / 100)
        }
      }
    },
    "3" = {
      levels[1] <- last_level + forecast_vals[1]
      if (h > 1) {
        for (i in 2:h) {
          levels[i] <- levels[i - 1] + forecast_vals[i]
        }
      }
    },
    "4" = {
      # Need last two levels for second-difference back-transform
      levels <- forecast_vals
    }
  )
  levels
}

#' Build the estimation panel
#'
#' Combines target variables and panel variables into a wide matrix,
#' trimming to the common sample.
#' @param target_df Transformed tidy data frame of target variables
#' @param panel_df Transformed tidy data frame of panel variables
#' @return List with:
#'   - targets: matrix of target variables (rows = dates, named columns)
#'   - panel: matrix of panel variables (rows = dates, named columns)
#'   - dates: vector of quarter-end dates
build_panel <- function(target_df, panel_df) {
  # Deduplicate: keep last value per date-series combination
  target_df <- target_df |>
    dplyr::distinct(date, series, .keep_all = TRUE)
  panel_df <- panel_df |>
    dplyr::distinct(date, series, .keep_all = TRUE)

  # Drop any panel series with fewer than 20 observations
  series_counts <- panel_df |>
    dplyr::filter(is.finite(value)) |>
    dplyr::count(series)
  keep_series <- series_counts$series[series_counts$n >= 20]
  panel_df <- panel_df |> dplyr::filter(series %in% keep_series)

  # Pivot to wide
  target_wide <- target_df |>
    tidyr::pivot_wider(names_from = series, values_from = value)

  panel_wide <- panel_df |>
    tidyr::pivot_wider(names_from = series, values_from = value)

  # Merge on date
  combined <- dplyr::inner_join(target_wide, panel_wide, by = "date")

  # Replace Inf/NaN with NA
  combined <- combined |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                 ~dplyr::if_else(is.finite(.), ., NA_real_)))

  target_cols <- setdiff(names(target_wide), "date")
  panel_cols <- setdiff(names(panel_wide), "date")

  # Step 1: Drop rows where any TARGET variable is NA
  combined <- combined |>
    tidyr::drop_na(dplyr::all_of(target_cols))

  # Step 2: Drop panel columns with >20% missing in remaining rows
  n_rows <- nrow(combined)
  na_frac <- colSums(is.na(combined[, panel_cols, drop = FALSE])) / n_rows
  keep_panels <- names(na_frac[na_frac <= 0.20])
  panel_cols <- intersect(panel_cols, keep_panels)

  # Step 3: Drop remaining rows with any NA in kept panel columns
  combined <- combined |>
    tidyr::drop_na(dplyr::all_of(panel_cols))

  dates <- combined$date

  list(
    targets = as.matrix(combined[, target_cols, drop = FALSE]),
    panel   = as.matrix(combined[, panel_cols, drop = FALSE]),
    dates   = dates
  )
}
