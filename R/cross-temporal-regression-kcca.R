#' Single Cross-Temporal Regression With Kernel Canonical Correlation Analysis (KCCA)
#'
#' This function applies Kernel Canonical Correlation Analysis (KCCA) to determine 
#' the correlation between returns and volume. Then, it performs cross-temporal regression 
#' on the given day and returns an S-Score, which measures the mean reversion tendency for each stock.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param t An integer, the current day for which to calculate S-Scores.
#' @param h An integer, the number of recent historical days to be used for returns.
#' @param hv An integer, the number of recent historical days to be used for volume.
#' @param l An integer, the number of preceding days used for the model.
#' @param nr_c_r An integer, the number of components used for returns in KCCA.
#' @param nr_c_v An integer, the number of components used for volume in KCCA.
#' @param b_sensitivity A numeric value, sensitivity for checking if model b coefficient is less than 1 minus this value.
#'
#' @return A list containing:
#' * `s`: The computed S-Score for each stock.
#' * `is_mean_reverting`: A logical vector indicating whether each stock is mean reverting.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' vol <- matrix(rnorm(25), 5, 5)
#' t_val <- 6
#' historical_ret <- 4
#' historical_vol <- 4
#' preceding <- 1
#' kcca_components_ret <- 2
#' kcca_components_vol <- 2
#' sensitivity <- 0.1
#' #Use the function
#' singleCrossTemporalRegressionWithKCCA(ret, vol, t_val, historical_ret, historical_vol, preceding, kcca_components_ret, kcca_components_vol, sensitivity)
singleCrossTemporalRegressionWithKCCA <- function(returns, volume, t, h, hv, l, nr_c_r, nr_c_v, b_sensitivity) {
  x <- as.matrix(returns[, (t - h):(t - 1)])
  y <- as.matrix(volume[, (t - hv):(t - 1)])

  c <- kcca(x, y, ncomps = max(c(nr_c_r, nr_c_v)))

  x_coeff <- c@xcoef[, 1:nr_c_r]
  y_coeff <- c@ycoef[, 1:nr_c_v]

  x_coeff <- x_coeff / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)
  y_coeff <- y_coeff / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)

  portfolio <- cbind(x_coeff, y_coeff)
  portfolio_returns <- t(returns[(t - l):(t - 1)]) %*% portfolio

  models <- lapply(1:nrow(returns), function(i) {
    y <- unlist(returns[i, (t - l):(t - 1)])
    return(lm(as.numeric(y) ~ portfolio_returns))
  })

  coefficients <- estimateCoefficeients(models, b_sensitivity = b_sensitivity)

  s <- numeric(nrow(returns))
  mean_reverting_indices <- coefficients$is_mean_reverting == 1 # mean reversion
  s[index] <- -coefficients$m[mean_reverting_indices] / sqrt(coefficients$sigma_eq_squared[mean_reverting_indices])

  # RETURN
  return(list(
    s = s,
    is_mean_reverting = coefficients$is_mean_reverting
  ))
}

#' Cross-Temporal Regression With Kernel Canonical Correlation Analysis (KCCA)
#'
#' This function performs cross-temporal regression on a range of days using KCCA analysis.
#' It is a parallelized process that calculates S-Scores for each day and each stock based on volume-weighted historical returns.
#' KCCA is applied to determine the correlation between returns and volume.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param start An integer, the start of the date range for which to calculate S-Scores.
#' @param end An integer, the end of the date range for which to calculate S-Scores.
#' @param h An integer, the number of recent historical days to be used for returns.
#' @param hv An integer, the number of recent historical days to be used for volume.
#' @param l An integer, the number of preceding days used for the model.
#' @param nr_c_r An integer, the number of components used for returns in KCCA.
#' @param nr_c_v An integer, the number of components used for volume in KCCA.
#' @param d An integer, the width for rolling mean calculation on volume.
#' @param b_sensitivity A numeric value, sensitivity for checking if model b coefficient is less than 1 minus this value.
#'
#' @return A matrix of S-Scores where each row corresponds to a stock and each column corresponds to a date in the given `start:end` range.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' vol <- matrix(rnorm(25), 5, 5)
#' start_range <- 2
#' end_range <- 5
#' historical_ret <- 4
#' historical_vol <- 4
#' preceding <- 1
#' kcca_components_ret <- 2
#' kcca_components_vol <- 2
#' rolling_mean_width <- 2
#' sensitivity <- 0.1
#' #Use the function
#' crossTemporalRegressionWithKCCA(ret, vol, start_range, end_range, historical_ret, historical_vol, preceding, kcca_components_ret, kcca_components_vol, rolling_mean_width, sensitivity)
#' 
crossTemporalRegressionWithKCCA <- function(returns, volume, start, end, h, hv, l, nr_c_r, nr_c_v, d, b_sensitivity) {
  standardised_volume <- volume / t(rolling_mean(t(as.matrix(volume)), width = d))

  global_vars <- c("singleCrossTemporalRegressionWithKCCA", "estimateCoefficeients")
  local_vars <- c("returns", "h", "hv", "l", "nr_c_r", "nr_c_v", "b_sensitivity", "standardised_volume")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  clusterCall(cl, function() library("kernlab"))
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())

  predictions <- snow::parSapply(cl, start:end, function(t) {
    s <- singleCrossTemporalRegressionWithKCCA(
      returns = returns, volume = standardised_volume,
      t = t, h = h, hv = hv,
      l = l, nr_c_r = nr_c_r, nr_c_v = nr_c_v,
      b_sensitivity = b_sensitivity
    )
    p <- -s_scores
    return(p)
  })

  snow::stopCluster(cl)

  rownames(predictions) <- rownames(returns)
  colnames(predictions) <- start:end

  return(predictions)
}
