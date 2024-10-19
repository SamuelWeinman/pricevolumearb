#' Cross-Temporal Regression
#'
#' This function is a parallelized process that calculates S-Scores for each day in a given date 
#' range using only the historical data upto that date. The S-Score is a measure of mean reversion
#' tendency for each stock, and is calculated by `calculateSScore` function. Multiple packages 
#' are used to conduct parallel processing for efficiency.
#'
#' @param returns A numeric matrix, the returns data.
#' @param start An integer, the start of the date range for which to calculate S-Scores.
#' @param end An integer, the end of the date range for which to calculate S-Scores.
#' @param nr_pc An integer, the number of eigen portfolios to extract.
#' @param h An integer, the number of recent historical days to be used.
#' @param l An integer, the number of preceding days used for the model.
#' @param b_sensitivity A numeric value, sensitivity for checking if model b coefficient is less than 1 minus this value.
#'
#' @return A matrix of S-Scores where each row corresponds to a stock and each column corresponds 
#' to a date in the given `start:end` range.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(25), 5, 5)
#' nr_pc <- 2
#' h <- 4
#' l <- 1
#' b_sensitivity <- 0.1
#' start <- 2
#' end <- 5
#' #Use the function
#' crossTemporalRegression(returns, start, end, nr_pc, h, l, b_sensitivity)
crossTemporalRegression <- function(returns, start, end, nr_pc, h, l, b_sensitivity) {
  global_vars <- c(
    "calculateSScore",
    "estimateCoefficeients", "decompose", "extractEigenPortfolio",
    "constructEigenPortfolios", "constructRho"
  )

  local_vars <- c("returns", "h", "l", "b_sensitivity", "nr_pc")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())



  s_scores <- snow::parSapply(cl, start:end, function(t) {
    scores <- calculateSScore(
      returns = returns[, 1:(t - 1)],
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
