# data_pull.R
# Functions to download Australian macroeconomic data from ABS and RBA

#' Pull key ABS target series
#'
#' Downloads GDP, CPI, and unemployment rate from ABS.
#' @return A tidy data frame with columns: date, series_id, series, value
pull_abs_data <- function() {
  # Helper: pick first matching series
  pick_first <- function(df, pattern) {
    matched <- df |>
      dplyr::filter(grepl(pattern, series, ignore.case = TRUE))
    if (nrow(matched) == 0) return(NULL)
    first_id <- matched$series_id[1]
    matched |> dplyr::filter(series_id == first_id)
  }

  # GDP: National Accounts, cat 5206.0, chain volume measures
  gdp_raw <- readabs::read_abs(cat_no = "5206.0",
                                tables = 2,
                                check_local = FALSE)
  gdp <- gdp_raw |>
    dplyr::filter(
      series_type == "Seasonally Adjusted",
      grepl("Gross domestic product", series, ignore.case = TRUE)
    )
  gdp <- gdp |>
    dplyr::filter(series_id == gdp$series_id[1]) |>
    dplyr::transmute(date, series = "gdp", value)

  # CPI: Consumer Price Index, All groups, Australia (series A2325846C)
  # Using read_abs_series for reliability — the catalogue tables have been
  # restructured and no longer carry the full quarterly CPI history.
  cpi_raw <- readabs::read_abs_series("A2325846C")
  cpi <- cpi_raw |>
    dplyr::transmute(date, series = "cpi", value)

  # Unemployment rate: Labour Force, cat 6202.0, seasonally adjusted
  unemp_raw <- readabs::read_abs(cat_no = "6202.0",
                                  tables = 1,
                                  check_local = FALSE)
  unemp <- unemp_raw |>
    dplyr::filter(
      series_type == "Seasonally Adjusted",
      grepl("^Unemployment rate.*Persons", series, ignore.case = TRUE)
    )
  unemp <- unemp |>
    dplyr::filter(series_id == unemp$series_id[1]) |>
    dplyr::transmute(date, series = "unemployment_rate", value)

  dplyr::bind_rows(gdp, cpi, unemp)
}

#' Pull RBA cash rate
#'
#' Downloads the RBA cash rate target from statistical table A2.
#' @return A tidy data frame with columns: date, series, value
pull_rba_data <- function() {
  rba_raw <- readrba::read_rba(table_no = "A2")

  # "New Cash Rate Target" gives the level after each change
  cash_changes <- rba_raw |>
    dplyr::filter(series == "New Cash Rate Target") |>
    dplyr::transmute(date, value) |>
    dplyr::arrange(date)

  # Create a daily date sequence, then aggregate to monthly
  date_seq <- seq.Date(
    from = min(cash_changes$date),
    to   = Sys.Date(),
    by   = "day"
  )
  daily <- data.frame(date = date_seq)

  # Join on exact date, then forward-fill
  cash_daily <- daily |>
    dplyr::left_join(cash_changes, by = "date") |>
    dplyr::arrange(date) |>
    tidyr::fill(value, .direction = "downup")

  # Aggregate to monthly (end-of-month value)
  cash_monthly <- cash_daily |>
    dplyr::mutate(
      year  = lubridate::year(date),
      month = lubridate::month(date)
    ) |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(value = dplyr::last(value), .groups = "drop") |>
    dplyr::mutate(
      date = as.Date(paste0(year, "-",
                             formatC(month, width = 2, flag = "0"),
                             "-01"))
    ) |>
    dplyr::select(date, value)

  cash_monthly |>
    dplyr::transmute(
      date   = date,
      series = "cash_rate",
      value  = value
    )
}

#' Pull broader panel data for factor extraction
#'
#' Fetches ~30-50 additional ABS/RBA series covering labour market,
#' housing, financial, trade, and production indicators.
#' @return A tidy data frame with columns: date, series, value
pull_panel_data <- function() {
  panel_list <- list()

  # Helper: extract first matching SA series from an ABS dataset
  extract_abs_sa <- function(raw_df, pattern, label) {
    matched <- raw_df |>
      dplyr::filter(
        series_type == "Seasonally Adjusted",
        grepl(pattern, series, ignore.case = TRUE)
      )
    if (nrow(matched) == 0) return(NULL)
    # Take the first matching series_id
    first_id <- matched$series_id[1]
    matched |>
      dplyr::filter(series_id == first_id) |>
      dplyr::transmute(date, series = label, value)
  }

  # --- Labour market indicators (cat 6202.0) ---
  lab_raw <- readabs::read_abs(cat_no = "6202.0", tables = 1,
                                check_local = FALSE)

  panel_list$employment <- extract_abs_sa(
    lab_raw, "^Employed total.*Persons", "employment")
  panel_list$fulltime <- extract_abs_sa(
    lab_raw, "Employed full-time.*Persons", "fulltime_employment")
  panel_list$parttime <- extract_abs_sa(
    lab_raw, "Employed part-time.*Persons", "parttime_employment")
  panel_list$participation <- extract_abs_sa(
    lab_raw, "^Participation rate.*Persons", "participation_rate")
  panel_list$labour_force <- extract_abs_sa(
    lab_raw, "^Labour force total.*Persons", "labour_force")

  # --- Wage Price Index (cat 6345.0) ---
  tryCatch({
    wpi_raw <- readabs::read_abs(cat_no = "6345.0", tables = 1,
                                  check_local = FALSE)
    # Use first SA index series
    panel_list$wpi <- extract_abs_sa(
      wpi_raw, "Total hourly.*all sectors|Wage Price Index", "wpi")
    if (is.null(panel_list$wpi)) {
      # Fallback: take first SA series from the table
      sa <- wpi_raw |> dplyr::filter(series_type == "Seasonally Adjusted")
      if (nrow(sa) > 0) {
        first_id <- sa$series_id[1]
        panel_list$wpi <- sa |>
          dplyr::filter(series_id == first_id) |>
          dplyr::transmute(date, series = "wpi", value)
      }
    }
  }, error = function(e) message("WPI download failed: ", e$message))

  # --- Retail trade (cat 8501.0) ---
  tryCatch({
    retail_raw <- readabs::read_abs(cat_no = "8501.0", tables = 1,
                                     check_local = FALSE)
    panel_list$retail <- extract_abs_sa(
      retail_raw, "Turnover.*Total", "retail_turnover")
  }, error = function(e) message("Retail download failed: ", e$message))

  # --- Building approvals (cat 8731.0) ---
  tryCatch({
    build_raw <- readabs::read_abs(cat_no = "8731.0", tables = 1,
                                    check_local = FALSE)
    panel_list$building <- extract_abs_sa(
      build_raw, "Total.*dwelling|Number of dwelling", "building_approvals")
  }, error = function(e) message("Building approvals download failed: ", e$message))

  # --- National accounts components (cat 5206.0) ---
  tryCatch({
    na_raw <- readabs::read_abs(cat_no = "5206.0", tables = 2,
                                 check_local = FALSE)
    # Chain volume series are labelled "Seasonally Adjusted" in this table
    panel_list$consumption <- extract_abs_sa(
      na_raw, "Final consumption.*Household|Household.*final consumption",
      "consumption")
    panel_list$investment <- extract_abs_sa(
      na_raw, "Gross fixed capital formation", "investment")
    panel_list$exports <- extract_abs_sa(
      na_raw, "^Exports of", "exports")
    panel_list$imports <- extract_abs_sa(
      na_raw, "^Imports of", "imports")
  }, error = function(e) message("National accounts download failed: ", e$message))

  # --- RBA financial indicators (table F1.1 - Interest rates) ---
  tryCatch({
    rates_raw <- readrba::read_rba(table_no = "F1.1")
    panel_list$bill90 <- rates_raw |>
      dplyr::filter(grepl("90-day", series, ignore.case = TRUE)) |>
      dplyr::transmute(date, series = "bill_rate_90d", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
  }, error = function(e) message("RBA F1.1 download failed: ", e$message))

  # --- RBA exchange rates (table F11.1) ---
  tryCatch({
    fx_raw <- readrba::read_rba(table_no = "F11.1")
    panel_list$twi <- fx_raw |>
      dplyr::filter(grepl("Trade-weighted", series, ignore.case = TRUE)) |>
      dplyr::transmute(date, series = "twi", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
    panel_list$aud_usd <- fx_raw |>
      dplyr::filter(grepl("United States", series, ignore.case = TRUE)) |>
      dplyr::transmute(date, series = "aud_usd", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
  }, error = function(e) message("RBA F11.1 download failed: ", e$message))

  # --- RBA monetary aggregates (table D3) ---
  tryCatch({
    money_raw <- readrba::read_rba(table_no = "D3")
    panel_list$m3 <- money_raw |>
      dplyr::filter(grepl("M3", series, ignore.case = TRUE)) |>
      dplyr::transmute(date, series = "m3", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
    panel_list$broad_money <- money_raw |>
      dplyr::filter(grepl("Broad money", series, ignore.case = TRUE)) |>
      dplyr::transmute(date, series = "broad_money", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
  }, error = function(e) message("RBA D3 download failed: ", e$message))

  # --- RBA credit (table D1) ---
  tryCatch({
    credit_raw <- readrba::read_rba(table_no = "D1")
    panel_list$credit <- credit_raw |>
      dplyr::filter(grepl("Total", series, ignore.case = TRUE)) |>
      dplyr::transmute(date, series = "total_credit", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
    panel_list$housing_credit <- credit_raw |>
      dplyr::filter(grepl("Housing", series, ignore.case = TRUE)) |>
      dplyr::transmute(date, series = "housing_credit", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
  }, error = function(e) message("RBA D1 download failed: ", e$message))

  # --- Merchandise trade (cat 5368.0) ---
  tryCatch({
    trade_raw <- readabs::read_abs(cat_no = "5368.0", tables = 1,
                                    check_local = FALSE)
    panel_list$trade_balance <- extract_abs_sa(
      trade_raw, "Balance", "trade_balance")
  }, error = function(e) message("Trade download failed: ", e$message))

  # --- Producer Price Index (cat 6427.0) ---
  tryCatch({
    ppi_raw <- readabs::read_abs(cat_no = "6427.0", tables = 1,
                                  check_local = FALSE)
    panel_list$ppi <- extract_abs_sa(
      ppi_raw, "Final demand|All groups", "ppi")
  }, error = function(e) message("PPI download failed: ", e$message))

  # --- RBA Commodity Prices (table I2) ---
  tryCatch({
    comm_raw <- readrba::read_rba(table_no = "I2")
    # Overall commodity price index (A$)
    panel_list$commodity_aud <- comm_raw |>
      dplyr::filter(series == "Commodity prices \u2013 A$") |>
      dplyr::transmute(date, series = "commodity_prices_aud", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
    # Bulk commodities (iron ore, coal, LNG) — A$
    panel_list$bulk_comm <- comm_raw |>
      dplyr::filter(series == "Bulk commodities prices \u2013 A$") |>
      dplyr::transmute(date, series = "bulk_commodity_prices", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
    # Rural commodity prices — A$
    panel_list$rural_comm <- comm_raw |>
      dplyr::filter(series == "Rural commodity prices \u2013 A$") |>
      dplyr::transmute(date, series = "rural_commodity_prices", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
    # Base metals prices — A$
    panel_list$base_metals <- comm_raw |>
      dplyr::filter(series == "Base metals prices \u2013 A$") |>
      dplyr::transmute(date, series = "base_metals_prices", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
    # Non-rural commodity prices — A$
    panel_list$nonrural_comm <- comm_raw |>
      dplyr::filter(series == "Non-rural commodity prices \u2013 A$") |>
      dplyr::transmute(date, series = "nonrural_commodity_prices", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
  }, error = function(e) message("RBA I2 download failed: ", e$message))

  # --- RBA International Trade (table I1) ---
  tryCatch({
    trade_rba <- readrba::read_rba(table_no = "I1")
    # Coal exports
    panel_list$coal_exports <- trade_rba |>
      dplyr::filter(series == "Coal exports") |>
      dplyr::transmute(date, series = "coal_exports", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
    # Metal ores exports
    panel_list$metal_exports <- trade_rba |>
      dplyr::filter(series == "Metal ores exports") |>
      dplyr::transmute(date, series = "metal_ores_exports", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
    # Resource exports (total)
    panel_list$resource_exports <- trade_rba |>
      dplyr::filter(series == "Resource exports") |>
      dplyr::transmute(date, series = "resource_exports", value) |>
      dplyr::distinct(date, .keep_all = TRUE)
  }, error = function(e) message("RBA I1 download failed: ", e$message))

  # --- Terms of trade (ABS 5206.0, table 1) ---
  tryCatch({
    na1_raw <- readabs::read_abs(cat_no = "5206.0", tables = 1,
                                  check_local = FALSE)
    # Terms of trade index, seasonally adjusted
    panel_list$tot <- na1_raw |>
      dplyr::filter(
        series_type == "Seasonally Adjusted",
        grepl("Terms of trade.*Index\\s*;", series),
        !grepl("Percentage", series)
      )
    if (nrow(panel_list$tot) > 0) {
      first_id <- panel_list$tot$series_id[1]
      panel_list$tot <- panel_list$tot |>
        dplyr::filter(series_id == first_id) |>
        dplyr::transmute(date, series = "terms_of_trade", value)
    } else {
      panel_list$tot <- NULL
    }
  }, error = function(e) message("Terms of trade download failed: ", e$message))

  # Remove NULLs (series that weren't found) and combine
  panel_list <- Filter(Negate(is.null), panel_list)
  dplyr::bind_rows(panel_list)
}
