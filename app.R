# app.R
# Australian Economy FAVAR Shiny App
# Factor-Augmented VAR with forecasts, IRFs, and FEVD

library(shiny)
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

# Target variable labels for display
TARGET_LABELS <- c(
  gdp               = "GDP Growth (%, annualised)",
  cpi               = "CPI Inflation (%, annualised)",
  unemployment_rate = "Unemployment Rate (%)",
  cash_rate         = "Cash Rate (%)"
)

TARGET_NAMES <- names(TARGET_LABELS)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Australian Economy FAVAR"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
                    class = "btn-primary btn-block"),
      uiOutput("cache_status"),
      hr(),
      helpText("Data sourced from ABS (readabs) and RBA (readrba)."),
      helpText("Model: PCA factors + VAR on target variables.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Forecasts",
          br(),
          fluidRow(
            column(6, plotOutput("fc_gdp", height = "320px")),
            column(6, plotOutput("fc_cpi", height = "320px"))
          ),
          fluidRow(
            column(6, plotOutput("fc_unemp", height = "320px")),
            column(6, plotOutput("fc_cash", height = "320px"))
          )
        ),
        tabPanel(
          "Impulse Responses",
          br(),
          fluidRow(
            column(4,
                   selectInput("irf_shock", "Shock variable",
                               choices = NULL)
            )
          ),
          plotOutput("irf_plot", height = "600px")
        ),
        tabPanel(
          "Variance Decomposition",
          br(),
          fluidRow(
            column(6, plotOutput("fevd_gdp", height = "350px")),
            column(6, plotOutput("fevd_cpi", height = "350px"))
          ),
          fluidRow(
            column(6, plotOutput("fevd_unemp", height = "350px")),
            column(6, plotOutput("fevd_cash", height = "350px"))
          )
        ),
        tabPanel(
          "Factor Loadings",
          br(),
          fluidRow(
            column(6, plotOutput("scree_plot", height = "350px")),
            column(6,
                   br(),
                   htmlOutput("factor_summary")
            )
          ),
          hr(),
          plotOutput("loadings_plot", height = "500px")
        )
      )
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
        withProgress(message = "No cache found. Downloading data...", {
          incProgress(0.1, detail = "Fetching ABS target series")
          abs_df <- pull_abs_data()
          incProgress(0.3, detail = "Fetching RBA cash rate")
          rba_df <- pull_rba_data()
          incProgress(0.5, detail = "Fetching panel data")
          panel_df <- pull_panel_data()
          rd <- list(targets = bind_rows(abs_df, rba_df), panel = panel_df)
          cache_save(rd)
          rv$raw <- rd
        })
      }
    }
  }) |> bindEvent(TRUE, once = TRUE)

  # Refresh button: download fresh data and update cache
  observeEvent(input$refresh, {
    withProgress(message = "Downloading fresh data...", {
      incProgress(0.1, detail = "Fetching ABS target series")
      abs_df <- pull_abs_data()
      incProgress(0.3, detail = "Fetching RBA cash rate")
      rba_df <- pull_rba_data()
      incProgress(0.5, detail = "Fetching panel data")
      panel_df <- pull_panel_data()
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
  output$fc_gdp <- renderPlot({
    fc <- forecasts()
    prep <- prepared()
    req(fc, prep)
    fc <- annualise_forecast(fc, "gdp")
    hist_df <- get_history("gdp")
    last_d <- max(prep$dates)
    plot_forecast(fc, "gdp", history_df = hist_df, last_date = last_d,
                  title = TARGET_LABELS["gdp"])
  })

  output$fc_cpi <- renderPlot({
    fc <- forecasts()
    prep <- prepared()
    req(fc, prep)
    fc <- annualise_forecast(fc, "cpi")
    hist_df <- get_history("cpi")
    last_d <- max(prep$dates)
    plot_forecast(fc, "cpi", history_df = hist_df, last_date = last_d,
                  title = TARGET_LABELS["cpi"])
  })

  output$fc_unemp <- renderPlot({
    fc <- forecasts()
    prep <- prepared()
    req(fc, prep)
    hist_df <- get_history("unemployment_rate")
    last_d <- max(prep$dates)
    plot_forecast(fc, "unemployment_rate", history_df = hist_df,
                  last_date = last_d,
                  title = TARGET_LABELS["unemployment_rate"])
  })

  output$fc_cash <- renderPlot({
    fc <- forecasts()
    prep <- prepared()
    req(fc, prep)
    hist_df <- get_history("cash_rate")
    last_d <- max(prep$dates)
    plot_forecast(fc, "cash_rate", history_df = hist_df,
                  last_date = last_d,
                  title = TARGET_LABELS["cash_rate"])
  })

  # ---- IRF plot ----
  output$irf_plot <- renderPlot({
    irf_obj <- irf_result()
    req(irf_obj)
    # Show responses of target variables only
    target_vars <- intersect(TARGET_NAMES,
                              colnames(irf_obj$irf[[input$irf_shock]]))
    # Also include factors if shock is a factor
    all_vars <- colnames(irf_obj$irf[[input$irf_shock]])
    resp_vars <- if (length(target_vars) > 0) target_vars else all_vars
    plot_irf(irf_obj, impulse_var = input$irf_shock,
             response_vars = resp_vars)
  })

  # ---- FEVD plots ----
  output$fevd_gdp <- renderPlot({
    fevd <- fevd_result()
    req(fevd)
    if ("gdp" %in% names(fevd)) plot_fevd(fevd, "gdp")
  })

  output$fevd_cpi <- renderPlot({
    fevd <- fevd_result()
    req(fevd)
    if ("cpi" %in% names(fevd)) plot_fevd(fevd, "cpi")
  })

  output$fevd_unemp <- renderPlot({
    fevd <- fevd_result()
    req(fevd)
    if ("unemployment_rate" %in% names(fevd))
      plot_fevd(fevd, "unemployment_rate")
  })

  output$fevd_cash <- renderPlot({
    fevd <- fevd_result()
    req(fevd)
    if ("cash_rate" %in% names(fevd)) plot_fevd(fevd, "cash_rate")
  })

  # ---- Factor Loadings tab ----
  output$loadings_plot <- renderPlot({
    mod <- favar_model()
    prep <- prepared()
    req(mod, prep)
    loadings_mat <- mod$factors$loadings
    series_names <- colnames(prep$built$panel)
    plot_loadings(loadings_mat, series_names = series_names)
  })

  output$scree_plot <- renderPlot({
    mod <- favar_model()
    req(mod)
    plot_scree(mod$factors$sdev, n_factors = mod$factors$n_factors)
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
