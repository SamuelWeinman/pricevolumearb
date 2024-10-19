#' Single Cross-Temporal Regression Overtrade
#'
#' This function performs cross-temporal regression with consideration for overtrading.
#' It standardizes volume using recent historical data, and then constructs the eigen portfolios 
#' based on this standardized volume. The function then decomposes the volume weighted returns into 
#' a mean-reverting model for each stock and calculates the S-Score.
#'
#' @param volume A numeric matrix, the volume data.
#' @param returns A numeric matrix, the returns data.
#' @param t An integer, the current day for which to calculate S-Scores.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc.V An integer, the number of eigen portfolios to construct based on volume.
#' @param nr_pc An integer, the number of eigen portfolios to construct based on returns.
#' @param alpha A numeric, the weight on overtrading term.
#' @param l An integer, the number of preceding days used for the model.
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
#' historical <- 4
#' preceding <- 1
#' eigen_portfolio_num_vol <- 2
#' eigen_portfolio_num_ret <- 2
#' alpha_val <- 0.1
#' sensitivity <- 0.1
#' #Use the function
#' singleCrossTemporalRegressionOvertrade(vol, ret, t_val, historical, eigen_portfolio_num_vol, eigen_portfolio_num_ret, alpha_val, preceding, sensitivity)
singleCrossTemporalRegressionOvertrade <- function(volume, returns, t, h, nr_pc.V, nr_pc, alpha, l, b_sensitivity) {
  standardised_volume <- volume[, (t - h):(t - 1)] / apply(volume[, (t - h):(t - 1)], 1, sum)

  eigen <- extractEigenPortfolio(standardised_volume, nr_pc.V)


  overtraded_log <- sapply(1:h, function(i) {
    y <- standardised_volume[, i]
    model <- lm(y ~ eigen$portfolio)
    return(model$residuals)
  })

  overtraded <- exp(alpha * overtraded_log)
  weighted_returns <- returns[, (t - h):(t - 1)] / overtraded

  models <- decompose(
    returns = weighted_returns,
    h = h,
    l = l,
    nr_pc = nr_pc
  )
  coefficients <- estimateCoefficeients(models, b_sensitivity)

  s <- numeric(nrow(returns))
  mean_reverting_indices <- coefficients$is_mean_reverting == 1
  s[mean_reverting_indices] <- -coefficients$m[index] / sqrt(coefficients$sigma_eq_squared[mean_reverting_indices])

  return(list(
    s = s,
    is_mean_reverting = coefficients$is_mean_reverting
  ))
}

#' Cross-Temporal Regression With Overtrade
#'
#' This function performs cross-temporal regression on a range of days with consideration for overtrading.
#' It is a parallelized process that calculates S-Scores for each day based on volume-weighted historical returns.
#' Overtrading is accounted for by adjusting the standardised volume with the `singleCrossTemporalRegressionOvertrade` function.
#'
#' @param volume A numeric matrix, the volume data.
#' @param returns A numeric matrix, the returns data.
#' @param start An integer, the start of the date range for which to calculate S-Scores.
#' @param end An integer, the end of the date range for which to calculate S-Scores.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc.V An integer, the number of eigen portfolios to extract based on volume.
#' @param nr_pc An integer, the number of eigen portfolios to extract based on returns.
#' @param alpha A numeric, the weight on overtrading term.
#' @param l An integer, the number of preceding days used for the model.
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
#' historical <- 4
#' preceding <- 1
#' eigen_portfolio_num_vol <- 2
#' eigen_portfolio_num_ret <- 2
#' alpha_val <- 0.1
#' sensitivity <- 0.1
#' #Use the function
#' crossTemporalRegressionOvertrade(vol, ret, start_range, end_range, historical, eigen_portfolio_num_vol, eigen_portfolio_num_ret, alpha_val, preceding, sensitivity)
#' 
crossTemporalRegressionOvertrade <- function(volume, returns, start, end, h, nr_pc.V, nr_pc, alpha, l, b_sensitivity) {
  global_vars <- c(
    "singleCrossTemporalRegressionOvertrade",
    "estimateCoefficeients", "decompose", "extractEigenPortfolio",
    "constructEigenPortfolios", "constructRho"
  )

  local_vars <- c(
    "returns", "volume",
    "h", "l", "b_sensitivity",
    "nr_pc.V", "nr_pc", "alpha", "b_sensitivity"
  )

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  parallel::clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())


  predictions <- snow::parSapply(cl, start:end, function(t) {
    s <- singleCrossTemporalRegressionOvertrade(
      volume = volume, returns = returns, t = t,
      h = h, nr_pc.V = nr_pc.V, nr_pc = nr_pc,
      alpha = alpha, l = l, b_sensitivity = b_sensitivity
    )

    return(-s$score)
  })

  snow::stopCluster(cl)
  rownames(predictions) <- rownames(returns)
  colnames(predictions) <- start:end

  return(predictions)
}
