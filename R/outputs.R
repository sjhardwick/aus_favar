# outputs.R
# Forecasting, IRF, FEVD computation and ggplot2 plotting functions

#' Produce FAVAR forecasts with bootstrap confidence bands
#'
#' @param model List returned by estimate_favar()
#' @param h Forecast horizon (quarters)
#' @param n_boot Number of bootstrap replications for confidence bands
#' @param ci Confidence interval width (default 0.70 and 0.90)
#' @return List with:
#'   - point: data frame of point forecasts (h x n_vars)
#'   - lower70, upper70: 70% bands
#'   - lower90, upper90: 90% bands
favar_forecast <- function(model, h = 8, n_boot = 500,
                           ci = c(0.70, 0.90)) {
  var_model <- model$var_model

  # Point forecast
  fc <- predict(var_model, n.ahead = h, ci = 0.95)

  # Extract point forecasts and bands for each variable
  var_names <- model$variable_names
  n_vars <- length(var_names)

  point_df <- data.frame(horizon = 1:h)
  lower70_df <- data.frame(horizon = 1:h)
  upper70_df <- data.frame(horizon = 1:h)
  lower90_df <- data.frame(horizon = 1:h)
  upper90_df <- data.frame(horizon = 1:h)

  for (v in var_names) {
    fc_v <- fc$fcst[[v]]
    point_df[[v]] <- fc_v[, "fcst"]

    # Use the predict() SE to construct bands at different CI levels
    se <- (fc_v[, "upper"] - fc_v[, "lower"]) / (2 * qnorm(0.975))

    z70 <- qnorm(0.85)
    z90 <- qnorm(0.95)

    lower70_df[[v]] <- fc_v[, "fcst"] - z70 * se
    upper70_df[[v]] <- fc_v[, "fcst"] + z70 * se
    lower90_df[[v]] <- fc_v[, "fcst"] - z90 * se
    upper90_df[[v]] <- fc_v[, "fcst"] + z90 * se
  }

  list(
    point   = point_df,
    lower70 = lower70_df,
    upper70 = upper70_df,
    lower90 = lower90_df,
    upper90 = upper90_df
  )
}

#' Compute orthogonalised impulse response functions
#'
#' @param model List returned by estimate_favar()
#' @param n_ahead Number of periods ahead
#' @param impulse Name of the impulse variable (NULL for all)
#' @param response Name of the response variable (NULL for all)
#' @param boot Whether to compute bootstrap confidence bands
#' @param n_boot Number of bootstrap replications
#' @param ci Confidence level
#' @return vars::irf object
favar_irf <- function(model, n_ahead = 20, impulse = NULL,
                      response = NULL, boot = TRUE,
                      n_boot = 500, ci = 0.95) {
  vars::irf(model$var_model,
            impulse    = impulse,
            response   = response,
            n.ahead    = n_ahead,
            ortho      = TRUE,
            boot       = boot,
            runs       = n_boot,
            ci         = ci)
}

#' Compute forecast error variance decomposition
#'
#' @param model List returned by estimate_favar()
#' @param n_ahead Number of periods ahead
#' @return vars::fevd object (list of matrices)
favar_fevd <- function(model, n_ahead = 20) {
  vars::fevd(model$var_model, n.ahead = n_ahead)
}

# Variables whose q/q rates should be annualised (* 4) for display
ANNUALISE_VARS <- c("gdp", "cpi")

#' Scale forecast object to annualised rates for selected variables
#'
#' @param forecast_obj List returned by favar_forecast()
#' @param variable Variable name
#' @return forecast_obj with values multiplied by 4 if variable is annualised
annualise_forecast <- function(forecast_obj, variable) {
  if (!(variable %in% ANNUALISE_VARS)) return(forecast_obj)
  for (slot in c("point", "lower70", "upper70", "lower90", "upper90")) {
    forecast_obj[[slot]][[variable]] <- forecast_obj[[slot]][[variable]] * 4
  }
  forecast_obj
}

# --- Plotting helpers ---

#' Plot fan chart forecast for a single target variable
#'
#' @param forecast_obj List returned by favar_forecast()
#' @param variable Name of the variable to plot
#' @param history_df Optional data frame with date and value columns for
#'   historical data
#' @param last_date Last date in the estimation sample
#' @param title Plot title
#' @return ggplot object
plot_forecast <- function(forecast_obj, variable, history_df = NULL,
                          last_date = NULL, title = NULL) {
  h <- nrow(forecast_obj$point)

  if (is.null(last_date)) {
    last_date <- Sys.Date()
  }

  # Create forecast dates (quarterly)
  fc_dates <- seq.Date(
    from = last_date + 1,
    by   = "quarter",
    length.out = h
  )

  fc_df <- data.frame(
    date    = fc_dates,
    point   = forecast_obj$point[[variable]],
    lower70 = forecast_obj$lower70[[variable]],
    upper70 = forecast_obj$upper70[[variable]],
    lower90 = forecast_obj$lower90[[variable]],
    upper90 = forecast_obj$upper90[[variable]]
  )

  if (is.null(title)) {
    title <- paste("Forecast:", variable)
  }

  p <- ggplot2::ggplot()

  # Historical data
  if (!is.null(history_df) && nrow(history_df) > 0) {
    p <- p + ggplot2::geom_line(
      data = history_df,
      ggplot2::aes(x = date, y = value),
      colour = "black", linewidth = 0.6
    )
  }

  # 90% band

  p <- p +
    ggplot2::geom_ribbon(
      data = fc_df,
      ggplot2::aes(x = date, ymin = lower90, ymax = upper90),
      fill = "#2171B5", alpha = 0.2
    ) +
    # 70% band
    ggplot2::geom_ribbon(
      data = fc_df,
      ggplot2::aes(x = date, ymin = lower70, ymax = upper70),
      fill = "#2171B5", alpha = 0.3
    ) +
    # Point forecast
    ggplot2::geom_line(
      data = fc_df,
      ggplot2::aes(x = date, y = point),
      colour = "#2171B5", linewidth = 0.8
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  p
}

#' Plot impulse response functions
#'
#' @param irf_obj Object returned by favar_irf()
#' @param impulse_var Name of the shock variable to plot
#' @param response_vars Character vector of response variable names
#' @return ggplot object
plot_irf <- function(irf_obj, impulse_var, response_vars = NULL) {
  irf_data <- irf_obj$irf[[impulse_var]]
  lower_data <- irf_obj$Lower[[impulse_var]]
  upper_data <- irf_obj$Upper[[impulse_var]]

  if (is.null(response_vars)) {
    response_vars <- colnames(irf_data)
  }

  n_ahead <- nrow(irf_data)
  horizon <- 0:(n_ahead - 1)

  plot_list <- list()
  for (rv in response_vars) {
    df <- data.frame(
      horizon  = horizon,
      irf      = irf_data[, rv],
      lower    = lower_data[, rv],
      upper    = upper_data[, rv]
    )
    plot_list[[rv]] <- df
  }

  plot_df <- dplyr::bind_rows(plot_list, .id = "response")

  ggplot2::ggplot(plot_df, ggplot2::aes(x = horizon)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      fill = "#2171B5", alpha = 0.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = irf),
      colour = "#2171B5", linewidth = 0.7
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::facet_wrap(~response, scales = "free_y", ncol = 2) +
    ggplot2::labs(
      title = paste("Impulse Response to:", impulse_var),
      x = "Quarters ahead",
      y = "Response"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      strip.text    = ggplot2::element_text(face = "bold"),
      panel.spacing = ggplot2::unit(1, "lines")
    )
}

#' Plot forecast error variance decomposition
#'
#' @param fevd_obj Object returned by favar_fevd()
#' @param variable Name of the variable whose FEVD to plot
#' @return ggplot object
plot_fevd <- function(fevd_obj, variable) {
  fevd_mat <- fevd_obj[[variable]]
  n_ahead <- nrow(fevd_mat)

  fevd_df <- as.data.frame(fevd_mat)
  fevd_df$horizon <- 1:n_ahead

  long_df <- tidyr::pivot_longer(
    fevd_df,
    cols      = -horizon,
    names_to  = "shock",
    values_to = "share"
  )

  # Colour palette
  n_shocks <- length(unique(long_df$shock))
  colours <- grDevices::hcl.colors(n_shocks, palette = "Dark 3")

  ggplot2::ggplot(long_df, ggplot2::aes(x = horizon, y = share, fill = shock)) +
    ggplot2::geom_area(alpha = 0.85) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = paste("FEVD:", variable),
      x     = "Quarters ahead",
      y     = "Share of forecast error variance",
      fill  = "Shock"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

#' Plot factor loadings as a heatmap
#'
#' @param loadings_matrix Matrix of factor loadings (series x factors)
#' @param series_names Character vector of panel series names
#' @param n_factors Number of factors to show (NULL = all)
#' @return ggplot object
plot_loadings <- function(loadings_matrix, series_names = NULL,
                          n_factors = NULL) {
  if (is.null(series_names)) {
    series_names <- rownames(loadings_matrix)
    if (is.null(series_names)) {
      series_names <- paste0("Series", seq_len(nrow(loadings_matrix)))
    }
  }

  k <- ncol(loadings_matrix)
  if (!is.null(n_factors)) k <- min(k, n_factors)

  loadings_sub <- loadings_matrix[, 1:k, drop = FALSE]
  colnames(loadings_sub) <- paste0("Factor ", 1:k)

  df <- as.data.frame(loadings_sub)
  df$series <- series_names
  # Order series by absolute loading on Factor 1 for readability
  df$series <- factor(df$series,
                       levels = df$series[order(abs(df[[1]]))])

  long_df <- tidyr::pivot_longer(
    df,
    cols      = -series,
    names_to  = "factor",
    values_to = "loading"
  )

  # Preserve factor ordering
  long_df$factor <- factor(long_df$factor,
                            levels = paste0("Factor ", 1:k))

  ggplot2::ggplot(long_df,
                   ggplot2::aes(x = factor, y = series, fill = loading)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(
      low = "#B2182B", mid = "white", high = "#2166AC",
      midpoint = 0, name = "Loading"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", loading)),
      size = 3, colour = "grey20"
    ) +
    ggplot2::labs(
      title = "Factor Loadings",
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title     = ggplot2::element_text(face = "bold"),
      axis.text.x    = ggplot2::element_text(angle = 0, hjust = 0.5),
      panel.grid     = ggplot2::element_blank()
    )
}

#' Plot variance explained by each factor
#'
#' @param sdev Standard deviations from PCA (one per component)
#' @param n_factors Number of factors to show
#' @return ggplot object
plot_scree <- function(sdev, n_factors = NULL) {
  var_explained <- sdev^2
  total_var <- sum(var_explained)
  pct <- var_explained / total_var * 100
  cum_pct <- cumsum(pct)

  k <- length(sdev)
  if (!is.null(n_factors)) k <- min(k, n_factors)

  df <- data.frame(
    factor  = factor(1:k),
    pct     = pct[1:k],
    cum_pct = cum_pct[1:k]
  )

  ggplot2::ggplot(df, ggplot2::aes(x = factor, y = pct)) +
    ggplot2::geom_col(fill = "#2171B5", alpha = 0.7, width = 0.6) +
    ggplot2::geom_line(
      ggplot2::aes(y = cum_pct, group = 1),
      colour = "#D94801", linewidth = 0.8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = cum_pct),
      colour = "#D94801", size = 2.5
    ) +
    ggplot2::scale_y_continuous(
      name = "% of variance",
      sec.axis = ggplot2::sec_axis(~., name = "Cumulative %")
    ) +
    ggplot2::labs(
      title = "Variance Explained by Factor",
      x = "Factor"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )
}
