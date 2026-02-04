# favar.R
# Factor extraction (PCA) and VAR estimation for FAVAR model

#' Select number of factors using Bai-Ng (2002) information criteria
#'
#' @param panel_matrix N x T matrix of standardised panel data
#' @param k_max Maximum number of factors to consider
#' @return Integer: selected number of factors
bai_ng_ic <- function(panel_matrix, k_max = 10) {
  n <- nrow(panel_matrix)
  p <- ncol(panel_matrix)
  k_max <- min(k_max, min(n, p) - 1)

  svd_res <- svd(panel_matrix, nu = k_max, nv = k_max)

  ic_vals <- numeric(k_max)
  total_var <- sum(panel_matrix^2) / (n * p)

  for (k in seq_len(k_max)) {
    factors <- svd_res$u[, 1:k, drop = FALSE] %*% diag(svd_res$d[1:k], nrow = k)
    loadings <- svd_res$v[, 1:k, drop = FALSE]
    fitted <- factors %*% t(loadings)
    residuals <- panel_matrix - fitted
    sigma2 <- sum(residuals^2) / (n * p)

    # IC_p2 criterion from Bai & Ng (2002)
    penalty <- k * ((n + p) / (n * p)) * log(min(n, p))
    ic_vals[k] <- log(sigma2) + penalty
  }

  which.min(ic_vals)
}

#' Extract factors from panel data via PCA
#'
#' @param panel_matrix Matrix of panel data (rows = time, cols = series)
#' @param n_factors Number of factors to extract; if NULL, use Bai-Ng IC
#' @return List with:
#'   - factors: matrix of extracted factors (T x n_factors)
#'   - loadings: matrix of factor loadings
#'   - n_factors: number of factors used
#'   - sdev: standard deviations of each component
#'   - center: column means used for standardisation
#'   - scale: column sds used for standardisation
extract_factors <- function(panel_matrix, n_factors = NULL) {
  # Standardise
  col_means <- colMeans(panel_matrix, na.rm = TRUE)
  col_sds <- apply(panel_matrix, 2, sd, na.rm = TRUE)
  # Avoid division by zero

  col_sds[col_sds < 1e-10] <- 1

  standardised <- scale(panel_matrix, center = col_means, scale = col_sds)

  # Select number of factors

  if (is.null(n_factors) || n_factors < 1) {
    n_factors <- bai_ng_ic(standardised)
  }

  # PCA via SVD
  pca_res <- prcomp(standardised, center = FALSE, scale. = FALSE,
                     rank. = n_factors)

  list(
    factors    = pca_res$x[, 1:n_factors, drop = FALSE],
    loadings   = pca_res$rotation[, 1:n_factors, drop = FALSE],
    n_factors  = n_factors,
    sdev       = pca_res$sdev[1:n_factors],
    center     = col_means,
    scale      = col_sds
  )
}

#' Estimate FAVAR model
#'
#' Combines extracted factors with target variables and estimates a VAR(p).
#' @param factors Matrix of factors (T x K)
#' @param targets Matrix of target variables (T x M)
#' @param p Lag order; if NULL, selected by AIC
#' @param ic Information criterion for lag selection ("AIC" or "SC")
#' @return List with:
#'   - var_model: fitted vars::VAR object
#'   - p: lag order used
#'   - variable_names: names of all variables in the system
estimate_favar <- function(factors, targets, p = NULL, ic = "AIC") {
  # Name the factor columns
  k <- ncol(factors)
  colnames(factors) <- paste0("Factor", seq_len(k))

  # Ensure target columns are named
  target_names <- colnames(targets)
  if (is.null(target_names)) {
    target_names <- paste0("Target", seq_len(ncol(targets)))
    colnames(targets) <- target_names
  }

  # Combine into a single data frame
  system_data <- as.data.frame(cbind(factors, targets))

  # Convert to ts for vars package
  system_ts <- ts(system_data, frequency = 4)

  # Determine lag order
  if (is.null(p) || p < 1) {
    max_lag <- max(1, min(8, floor((nrow(system_ts) - 1) / ncol(system_ts)) - 1))
    lag_select <- vars::VARselect(system_ts, lag.max = max_lag, type = "const")
    p <- lag_select$selection[ic]
    if (is.na(p) || p < 1) p <- 1L
    p <- max(1L, min(as.integer(p), max_lag))
  }

  var_model <- vars::VAR(system_ts, p = p, type = "const")

  list(
    var_model      = var_model,
    p              = p,
    variable_names = colnames(system_data)
  )
}
