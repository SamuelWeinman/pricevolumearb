#' Cross-Temporal Regression With Volume Weighting
#'
#' This function is a parallelized process that calculates S-Scores for each day in a given date 
#' range using only the volume weighted historical data up to that date. The S-Score is a measure of 
#' mean reversion tendency for each stock, and is calculated by the `calculateSScore` function.
#' The function uses multiple packages to conduct parallel processing for efficiency.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric vector, the volume data.
#' @param start An integer, the start of the date range for which to calculate S-Scores.
#' @param end An integer, the end of the date range for which to calculate S-Scores.
#' @param nr_pc An integer, the number of eigen portfolios to extract.
#' @param h An integer, the number of recent historical days to be used.
#' @param l An integer, the number of preceding days used for the model.
#' @param b_sensitivity A numeric value, sensitivity for checking if model b coefficient is less than 1 minus this value.
#' @param d An integer, the number of days used for volume weighting.
#' @param divide A logical value, indicating whether to divide returns by volume score. 
#'
#' @return A matrix of S-Scores where each row corresponds to a stock and each column corresponds 
#' to a date in the given `start:end` range. 
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' vol <- rnorm(5)
#' num_pc <- 2
#' historical <- 4
#' preceding <- 1
#' sensitivity <- 0.1
#' start_range <- 2
#' end_range <- 5
#' volume_days <- 2
#' divide_returns <- TRUE
#' #Use the function
#' crossTemporalRegressionWithVW(ret, vol, start_range, end_range, num_pc, historical, preceding, sensitivity, volume_days, divide_returns)
crossTemporalRegressionWithVW <- function(returns, volume, start, end, nr_pc, h, l, b_sensitivity, d, divide) {
  weighted_returns <- constructWeightedReturns(returns = returns, volume = volume, h = h, d = d, divide = divide)

  global_vars <- c(
    "calculateSScore",
    "estimateCoefficeients", "decompose", "extractEigenPortfolio",
    "constructEigenPortfolios", "constructRho", "constructWeightedReturns"
  )

  local_vars <- c("returns", "h", "l", "b_sensitivity", "w")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())

  s_scores <- snow::parSapply(cl, start:end, function(t) {
    scores <- calculateSScore(
      returns = w[, 1:(t_ - 1)],
      nr_pc = nr_pc,
      h = h, l = l,
      b_sensitivity = b_sensitivity
    )

    return(scores$s)
  })

  snow::stopCluster(cl)

  p <- -s_scores
  rownames(p) <- rownames(returns)
  colnames(p) <- start:end

  return(p)
}

#' Mapped Cross-Temporal Regression With Volume Weighting
#'
#' This function maps the volume data using given maps before performing cross-temporal 
#' regression with volume weighting. It calculates S-Scores for each day using volume weighted 
#' historical data for each map.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric vector, the volume data.
#' @param start An integer, the start of the date range for which to calculate S-Scores.
#' @param end An integer, the end of the date range for which to calculate S-Scores.
#' @param nr_pc An integer, the number of eigen portfolios to extract.
#' @param h An integer, the number of recent historical days to be used.
#' @param l An integer, the number of preceding days used for the model.
#' @param b_sensitivity A numeric value, sensitivity for checking if model b coefficient is less than 1 minus this value.
#' @param d An integer, the number of days used for volume weighting.
#' @param divide A logical value, indicating whether to divide returns by volume score.
#' @param maps A list of functions, each function is used to map the volume data.
#'
#' @return A list of S-Score matrices (from `crossTemporalRegressionWithVW`), one for each mapping function.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' vol <- rnorm(5)
#' num_pc <- 2
#' historical <- 4
#' preceding <- 1
#' sensitivity <- 0.1
#' start_range <- 2
#' end_range <- 5
#' volume_days <- 2
#' divide_returns <- TRUE
#' map_funcs <- list(sqrt, log)
#' #Use the function
#' mappedCrossTemporalRegressionWithVW(ret, vol, start_range, end_range, num_pc, historical, preceding, sensitivity, volume_days, divide_returns, map_funcs
mappedCrossTemporalRegressionWithVW <- function(returns, volume, start, end, nr_pc, h, l, b_sensitivity, d, divide, maps) {
  k <- length(maps)
  predictions <- list()

  for (k_i in 1:k) {
    map <- maps[[k]]
    mapped_volume <- map(volume)
    predictions[[k]] <- crossTemporalRegressionWithVW(
      returns = returns,
      volume = mapped_volume,
      start = start, end = end,
      nr_pc = nr_pc, h = h, l = l,
      b_sensitivity = b_sensitivity,
      d = d, divide = divide
    )
  }
  return(predictions)
}
