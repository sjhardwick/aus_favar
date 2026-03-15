# app.R
# Australian Economy FAVAR Shiny App
# Factor-Augmented VAR with forecasts, IRFs, and FEVD

library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(vars)
library(readabs)
library(readrba)

# Source helper modules
source("R/data_pull.R")
source("R/data_prep.R")
source("R/favar.R")
source("R/outputs.R")
source("R/cache.R")

# Plotly helper: hide confidence-band hover, label lines, remove modebar
plotly_clean <- function(p, hide_fills = FALSE, label_lines = FALSE,
                         date_hover = FALSE) {
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]
    has_fill <- !is.null(tr$fill) && nzchar(tr$fill)
    if (hide_fills && has_fill) {
      p$x$data[[i]]$hoverinfo <- "skip"
    } else if (label_lines && !is.null(tr$mode) && grepl("lines", tr$mode)) {
      col <- if (!is.null(tr$line$color)) tr$line$color else ""
      lbl <- if (grepl("rgba\\(0,0,0", col)) "Historical" else "Forecast"
      p$x$data[[i]]$name <- lbl
      if (date_hover && is.numeric(tr$x)) {
        # Format days-since-epoch as "YYYY-QN" and attach as customdata
        dates <- as.Date(tr$x, origin = "1970-01-01")
        p$x$data[[i]]$customdata <- paste0(
          format(dates, "%Y"), "-Q",
          (as.numeric(format(dates, "%m")) - 1) %/% 3 + 1
        )
        p$x$data[[i]]$hovertemplate <- paste0(
          "%{customdata}<br>%{y:.1f}<extra>", lbl, "</extra>"
        )
      } else {
        p$x$data[[i]]$hovertemplate <- paste0("%{y:.1f}<extra>", lbl, "</extra>")
      }
    }
  }
  # "closest" for date charts avoids the numeric axis hover label
  # that "x" / "x unified" modes produce from raw day-since-epoch values
  if (date_hover) {
    p <- p |> layout(hovermode = "closest")
  } else {
    p <- p |> layout(hovermode = "x unified")
  }
  p |> config(displayModeBar = FALSE)
}

# Target variable labels for display
TARGET_LABELS <- c(
  gdp               = "GDP Growth (%, annualised)",
  cpi               = "CPI Inflation (%, annualised)",
  unemployment_rate = "Unemployment Rate (%)",
  cash_rate         = "Cash Rate (%)"
)

TARGET_NAMES <- names(TARGET_LABELS)

# ---- UI ----
ui <- page_sidebar(
  title = "Australian Economy FAVAR",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    primary = "#2171B5"
  ),
  sidebar = sidebar(
    width = 300,
    h4("Model Settings"),
    sliderInput("horizon", "Forecast horizon (quarters)",
                min = 1, max = 12, value = 8, step = 1),
    radioButtons("n_factors_mode", "Number of factors",
                 choices = c("Auto (Bai-Ng IC)" = "auto",
                             "Manual" = "manual"),
                 selected = "auto"),
    conditionalPanel(
      condition = "input.n_factors_mode == 'manual'",
      sliderInput("n_factors", "Factors", min = 1, max = 10,
                  value = 3, step = 1)
    ),
    radioButtons("lag_mode", "VAR lag order",
                 choices = c("Auto (AIC)" = "auto",
                             "Manual" = "manual"),
                 selected = "auto"),
    conditionalPanel(
      condition = "input.lag_mode == 'manual'",
      sliderInput("var_lags", "Lags", min = 1, max = 8,
                  value = 2, step = 1)
    ),
    hr(),
    actionButton("refresh", "Refresh Data (Live Download)",
                  icon = icon("sync"),
                  class = "btn-primary w-100"),
    uiOutput("cache_status"),
    hr(),
    helpText("Data sourced from ABS (readabs) and RBA (readrba)."),
    helpText("Model: PCA factors + VAR on target variables.")
  ),

  tags$style(HTML("
    .card-header { font-weight: 600; font-size: 0.95rem; }
    @media (max-width: 575.98px) {
      .card { margin-bottom: 0.5rem; }
      .card-header { font-size: 0.85rem; }
    }
  ")),

  navset_card_tab(
    id = "tabs",
    nav_panel(
      "Forecasts",
      layout_columns(
        col_widths = bslib::breakpoints(sm = 6, xs = 12),
        card(card_header(TARGET_LABELS["gdp"]),
             plotlyOutput("fc_gdp", height = "280px")),
        card(card_header(TARGET_LABELS["cpi"]),
             plotlyOutput("fc_cpi", height = "280px")),
        card(card_header(TARGET_LABELS["unemployment_rate"]),
             plotlyOutput("fc_unemp", height = "280px")),
        card(card_header(TARGET_LABELS["cash_rate"]),
             plotlyOutput("fc_cash", height = "280px"))
      )
    ),
    nav_panel(
      "Impulse Responses",
      layout_columns(
        col_widths = bslib::breakpoints(sm = 4, xs = 12),
        selectInput("irf_shock", "Shock variable", choices = NULL)
      ),
      card(
        card_header(textOutput("irf_title", inline = TRUE)),
        plotlyOutput("irf_plot", height = "600px")
      )
    ),
    nav_panel(
      "Variance Decomposition",
      layout_columns(
        col_widths = bslib::breakpoints(sm = 6, xs = 12),
        card(card_header("FEVD: GDP Growth"),
             plotlyOutput("fevd_gdp", height = "300px")),
        card(card_header("FEVD: CPI Inflation"),
             plotlyOutput("fevd_cpi", height = "300px")),
        card(card_header("FEVD: Unemployment Rate"),
             plotlyOutput("fevd_unemp", height = "300px")),
        card(card_header("FEVD: Cash Rate"),
             plotlyOutput("fevd_cash", height = "300px"))
      )
    ),
    nav_panel(
      "Factor Loadings",
      layout_columns(
        col_widths = bslib::breakpoints(sm = 6, xs = 12),
        card(card_header("Variance Explained"),
             plotlyOutput("scree_plot", height = "300px")),
        card(card_header("Model Summary"),
             htmlOutput("factor_summary"))
      ),
      hr(),
      card(card_header("Factor Loadings"),
           plotlyOutput("loadings_plot", height = "500px"))
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # Reactive values to hold raw data
  rv <- reactiveValues(raw = NULL)

  # On startup: load from cache if available, otherwise download

  observe({
    if (is.null(rv$raw)) {
      cached <- cache_load()
      if (!is.null(cached)) {
        rv$raw <- cached
      } else {
        n_steps <- 20
        step <- 0
        tick <- function(detail = "") {
          step <<- step + 1
          incProgress(1 / n_steps, detail = detail)
        }
        withProgress(message = "No cache found. Downloading data...", max = 1, {
          abs_df <- pull_abs_data(progress = tick)
          rba_df <- pull_rba_data(progress = tick)
          panel_df <- pull_panel_data(progress = tick)
          tick(detail = "Saving cache")
          rd <- list(targets = bind_rows(abs_df, rba_df), panel = panel_df)
          cache_save(rd)
          rv$raw <- rd
        })
      }
    }
  }) |> bindEvent(TRUE, once = TRUE)

  # Refresh button: download fresh data and update cache
  observeEvent(input$refresh, {
    n_steps <- 20
    step <- 0
    tick <- function(detail = "") {
      step <<- step + 1
      incProgress(1 / n_steps, detail = detail)
    }
    withProgress(message = "Downloading fresh data...", max = 1, {
      abs_df <- pull_abs_data(progress = tick)
      rba_df <- pull_rba_data(progress = tick)
      panel_df <- pull_panel_data(progress = tick)
      tick(detail = "Saving cache")
      rd <- list(targets = bind_rows(abs_df, rba_df), panel = panel_df)
      cache_save(rd)
      rv$raw <- rd
    })
  })

  # Expose raw data as a reactive for downstream consumers
  raw_data <- reactive({ rv$raw })

  # Cache status display
  output$cache_status <- renderUI({
    # Re-render when raw data changes
    raw_data()
    helpText(paste("Data cached:", cache_age_label()))
  })

  # Reactive: prepared data
  prepared <- reactive({
    rd <- raw_data()
    req(rd)

    withProgress(message = "Preparing data...", {
      # Align to quarterly
      incProgress(0.3, detail = "Aligning to quarterly frequency")
      targets_q <- align_quarterly(rd$targets)
      panel_q <- align_quarterly(rd$panel)

      # Store raw quarterly levels for back-transformation / history plots
      raw_targets_wide <- targets_q |>
        pivot_wider(names_from = series, values_from = value) |>
        arrange(date)

      # Transform to stationary
      incProgress(0.6, detail = "Applying stationarity transforms")
      targets_trans <- transform_stationary(targets_q)
      panel_trans <- transform_stationary(panel_q)

      # Build panel
      incProgress(0.9, detail = "Building estimation panel")
      built <- build_panel(targets_trans$data, panel_trans$data)

      validate(
        need(nrow(built$targets) >= 20,
             "Not enough observations after alignment. Check data sources.")
      )

      list(
        built      = built,
        transforms = targets_trans$transforms,
        raw_wide   = raw_targets_wide,
        dates      = built$dates
      )
    })
  })

  # Reactive: FAVAR model
  favar_model <- reactive({
    prep <- prepared()
    req(prep)

    withProgress(message = "Estimating FAVAR...", {
      # Factor extraction
      n_fac <- if (input$n_factors_mode == "manual") input$n_factors else NULL
      incProgress(0.3, detail = "Extracting factors (PCA)")
      fac <- extract_factors(prep$built$panel, n_factors = n_fac)

      # VAR estimation
      p <- if (input$lag_mode == "manual") input$var_lags else NULL
      incProgress(0.7, detail = paste0("Estimating VAR(", ifelse(is.null(p), "auto", p), ")"))
      mod <- estimate_favar(fac$factors, prep$built$targets, p = p)

      list(
        model   = mod,
        factors = fac
      )
    })
  })

  # Update shock variable choices when model is estimated
  observe({
    mod <- favar_model()
    req(mod)
    var_names <- mod$model$variable_names
    updateSelectInput(session, "irf_shock", choices = var_names,
                      selected = var_names[length(var_names)])
  })

  # Reactive: forecasts
  forecasts <- reactive({
    mod <- favar_model()
    req(mod)
    favar_forecast(mod$model, h = input$horizon)
  })

  # Reactive: IRF
  irf_result <- reactive({
    mod <- favar_model()
    req(mod, input$irf_shock)
    favar_irf(mod$model, n_ahead = 20, impulse = input$irf_shock)
  })

  # Reactive: FEVD
  fevd_result <- reactive({
    mod <- favar_model()
    req(mod)
    favar_fevd(mod$model, n_ahead = 20)
  })

  # Helper: get history for a target variable (transformed scale)
  # Annualises GDP and CPI (multiply q/q by 4)
  get_history <- function(variable) {
    prep <- prepared()
    req(prep)
    idx <- which(colnames(prep$built$targets) == variable)
    if (length(idx) == 0) return(NULL)
    vals <- prep$built$targets[, idx]
    if (variable %in% ANNUALISE_VARS) vals <- vals * 4
    data.frame(
      date  = prep$dates,
      value = vals
    )
  }

  # ---- Forecast plots ----
  output$fc_gdp <- renderPlotly({
    fc <- forecasts()
    prep <- prepared()
    req(fc, prep)
    fc <- annualise_forecast(fc, "gdp")
    hist_df <- get_history("gdp")
    last_d <- max(prep$dates)
    p <- plot_forecast(fc, "gdp", history_df = hist_df, last_date = last_d)
    ggplotly(p) |> plotly_clean(hide_fills = TRUE, label_lines = TRUE, date_hover = TRUE)
  })

  output$fc_cpi <- renderPlotly({
    fc <- forecasts()
    prep <- prepared()
    req(fc, prep)
    fc <- annualise_forecast(fc, "cpi")
    hist_df <- get_history("cpi")
    last_d <- max(prep$dates)
    p <- plot_forecast(fc, "cpi", history_df = hist_df, last_date = last_d)
    ggplotly(p) |> plotly_clean(hide_fills = TRUE, label_lines = TRUE, date_hover = TRUE)
  })

  output$fc_unemp <- renderPlotly({
    fc <- forecasts()
    prep <- prepared()
    req(fc, prep)
    hist_df <- get_history("unemployment_rate")
    last_d <- max(prep$dates)
    p <- plot_forecast(fc, "unemployment_rate", history_df = hist_df,
                       last_date = last_d)
    ggplotly(p) |> plotly_clean(hide_fills = TRUE, label_lines = TRUE, date_hover = TRUE)
  })

  output$fc_cash <- renderPlotly({
    fc <- forecasts()
    prep <- prepared()
    req(fc, prep)
    hist_df <- get_history("cash_rate")
    last_d <- max(prep$dates)
    p <- plot_forecast(fc, "cash_rate", history_df = hist_df,
                       last_date = last_d)
    ggplotly(p) |> plotly_clean(hide_fills = TRUE, label_lines = TRUE, date_hover = TRUE)
  })

  # ---- IRF plot ----
  output$irf_title <- renderText({
    req(input$irf_shock)
    paste("Impulse Response to:", input$irf_shock)
  })

  output$irf_plot <- renderPlotly({
    irf_obj <- irf_result()
    req(irf_obj)
    # Show responses of target variables only
    target_vars <- intersect(TARGET_NAMES,
                              colnames(irf_obj$irf[[input$irf_shock]]))
    # Also include factors if shock is a factor
    all_vars <- colnames(irf_obj$irf[[input$irf_shock]])
    resp_vars <- if (length(target_vars) > 0) target_vars else all_vars
    p <- plot_irf(irf_obj, impulse_var = input$irf_shock,
                  response_vars = resp_vars)
    ggplotly(p) |> plotly_clean(hide_fills = TRUE)
  })

  # ---- FEVD plots ----
  output$fevd_gdp <- renderPlotly({
    fevd <- fevd_result()
    req(fevd, "gdp" %in% names(fevd))
    ggplotly(plot_fevd(fevd, "gdp")) |> plotly_clean()
  })

  output$fevd_cpi <- renderPlotly({
    fevd <- fevd_result()
    req(fevd, "cpi" %in% names(fevd))
    ggplotly(plot_fevd(fevd, "cpi")) |> plotly_clean()
  })

  output$fevd_unemp <- renderPlotly({
    fevd <- fevd_result()
    req(fevd, "unemployment_rate" %in% names(fevd))
    ggplotly(plot_fevd(fevd, "unemployment_rate")) |> plotly_clean()
  })

  output$fevd_cash <- renderPlotly({
    fevd <- fevd_result()
    req(fevd, "cash_rate" %in% names(fevd))
    ggplotly(plot_fevd(fevd, "cash_rate")) |> plotly_clean()
  })

  # ---- Factor Loadings tab ----
  output$loadings_plot <- renderPlotly({
    mod <- favar_model()
    prep <- prepared()
    req(mod, prep)
    loadings_mat <- mod$factors$loadings
    series_names <- colnames(prep$built$panel)
    ggplotly(plot_loadings(loadings_mat, series_names = series_names)) |>
      plotly_clean()
  })

  output$scree_plot <- renderPlotly({
    mod <- favar_model()
    req(mod)
    ggplotly(plot_scree(mod$factors$sdev, n_factors = mod$factors$n_factors)) |>
      plotly_clean()
  })

  output$factor_summary <- renderUI({
    mod <- favar_model()
    prep <- prepared()
    req(mod, prep)

    n_fac <- mod$factors$n_factors
    n_obs <- nrow(prep$built$targets)
    n_panel <- ncol(prep$built$panel)
    var_explained <- mod$factors$sdev^2
    total_var <- sum(var_explained)
    cum_pct <- cumsum(var_explained) / total_var * 100

    lines <- paste0(
      "<b>Model summary</b><br>",
      "Observations: ", n_obs, " quarters<br>",
      "Panel series: ", n_panel, "<br>",
      "Factors extracted: ", n_fac, "<br>",
      "VAR lag order: ", mod$model$p, "<br>",
      "<br><b>Cumulative variance explained</b><br>"
    )
    for (i in seq_len(n_fac)) {
      lines <- paste0(lines,
        "Factor ", i, ": ", sprintf("%.1f%%", cum_pct[i]), "<br>"
      )
    }

    HTML(lines)
  })
}

shinyApp(ui, server)
