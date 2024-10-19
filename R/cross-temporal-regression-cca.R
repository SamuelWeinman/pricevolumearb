#' Single Cross-Temporal Regression With Canonical Correlation Analysis (CCA)
#'
#' This function applies Canonical Correlation Analysis (CCA) to determine 
#' the correlation between returns and standardised volume. Then, it performs cross-temporal regression 
#' on the given day and returns an S-Score, which measures the mean reversion tendency for each stock.
#'
#' @param returns A numeric matrix, the returns data.
#' @param standardised_volume A numeric matrix, the standardised_volume data.
#' @param t An integer, the current day for which to calculate S-Scores.
#' @param h An integer, the number of recent historical days to be used for returns.
#' @param hv An integer, the number of recent historical days to be used for standardised volume.
#' @param l An integer, the number of preceding days used for the model.
#' @param nr_c_r An integer, the number of components used for returns in CCA.
#' @param nr_c_v An integer, the number of components used for standardised volume in CCA.
#' @param b_sensitivity A numeric value, sensitivity for checking if model b coefficient is less than 1 minus this value.
#'
#' @return A list containing:
#' * `s`: The computed S-Score for each stock.
#' * `is_mean_reverting`: A logical vector indicating whether each stock is mean reverting.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(25), 5, 5)
#' volume <- matrix(rnorm(25), 5, 5)
#' standardised_volume <- vol / rowMeans(vol)
#' t <- 6
#' h <- 4
#' hv <- 4
#' l <- 1
#' nr_c_r <- 2
#' nr_c_v <- 2
#' b_sensitivity <- 0.1
#' #Use the function
#' singleCrossTemporalRegressionCCA(returns, standardised_volume, t, h, hv, l, nr_c_r, nr_c_v, b_sensitivity)
singleCrossTemporalRegressionCCA <- function(returns, standardised_volume, t, h, hv, l, nr_c_r, nr_c_v, b_sensitivity) {
  x <- returns[, (t - h):(t - 1)]
  y <- standardised_volume[, (t - hv):(t - 1)]

  c <- cc(x, y)

  x <- as.matrix(x) %*% as.matrix(c$xcoef)
  y <- as.matrix(y) %*% as.matrix(c$ycoef)

  x <- x[, 1:nr_c_r]
  y <- y[, 1:nr_c_r]

  x <- x / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)
  y <- y / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)

  portfolios <- cbind(x, y)
  portfolio_returns <- t(returns[(t - L):(t - 1)]) %*% portfolios

  n <- nrow(returns)
  models <- lapply(1:n, function(i) {
    return(lm(as.numeric(unlist(returns[i, (t - L):(t - 1)])) ~ portfolio_returns))
  })

  coefficients <- estimateCoefficeients(models, b_sensitivity = b_sensitivity)

  s <- numeric(nrow(returns))
  index <- coefficients$is_mean_reverting == 1
  s[index] <- -coefficients$m[index] / sqrt(coefficients$sigma_eq_squared[index])

  return(list(
    s = s,
    is_mean_reverting = coefficients$is_mean_reverting
  ))
}

#' Cross-Temporal Regression With Canonical Correlation Analysis (CCA)
#'
#' This function performs cross-temporal regression on a range of days using CCA analysis.
#' It is a parallelized process that calculates S-Scores for each day based on volume-standardised 
#' historical returns. CCA is applied to determine the correlation between returns and standardised volume.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param start An integer, the start of the date range for which to calculate S-Scores.
#' @param end An integer, the end of the date range for which to calculate S-Scores.
#' @param h An integer, the number of recent historical days to be used for returns.
#' @param hv An integer, the number of recent historical days to be used for standardised volume.
#' @param l An integer, the number of preceding days used for the model.
#' @param nr_c_r An integer, the number of components used for returns in CCA.
#' @param nr_c_v An integer, the number of components used for standardised volume in CCA.
#' @param d An integer, the width for calculating the rolling mean of the volume.
#' @param b_sensitivity A numeric value, sensitivity for checking if model b coefficient is less than 1 minus this value.
#'
#' @return A matrix of S-Scores where each row corresponds to a stock and each column corresponds to a date in the given `start:end` range.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(25), 5, 5)
#' volume <- matrix(rnorm(25), 5, 5)
#' start <- 2
#' end <- 5
#' hurns <- 4
#' hv <- 4
#' l <- 1
#' nr_c_r <- 2
#' nr_c_v <- 2
#' d <- 2
#' b_sensitivity <- 0.1
#' #Use the function
#' crossTemporalRegressionCCA(returns, volume, start, end, h, hv, l, nr_c_r, nr_c_v, d, b_sensitivity)
crossTemporalRegressionCCA <- function(returns, volume, start, end, h, hv, l, nr_c_r, nr_c_v, d, b_sensitivity) {
  standardised_volume <- volume / t(roll_mean(t(as.matrix(volume)), width = d))

  global_vars <- c("singleCrossTemporalRegressionCCA", "estimateCoefficeients")
  local_vars <- c("returns", "h", "hv", "l", "nr_c_r", "nr_c_v", "b_sensitivity", "standardised_volume")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  parallel::clusterCall(cl, function() library("CCA"))
  parallel::clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir <- environment())

  predictions <- snow::parSapply(cl, start:end, function(t) {
    s <- singleCrossTemporalRegressionCCA(
      returns = returns, standardised_volume = standardised_volume,
      t = t, h = h, hv = hv,
      l = l, nr_c_r = nr_c_r, nr_c_v = nr_c_v,
      b_sensitivity = b_sensitivity
    )
    return(-s$s)
  })
  snow::stopCluster(cl)

  rownames(predictions) <- rownames(returns)
  colnames(predictions) <- start:end

  return(predictions)
}
