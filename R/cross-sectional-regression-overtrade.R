#' Single Cross-Sectional Regression for Overtraded Volume
#'
#' This function performs cross-sectional regression to predict returns, taking into account overtraded volume. 
#' It first standardises the volume and extracts an eigenportfolio. Then, it calculates the overtrade amounts 
#' by fitting a linear model between the standardised volume and the eigenportfolio. Following this, it adjusts 
#' the returns by the exponential of the overtrade amounts, and fits another linear model between the adjusted 
#' returns and a new eigenportfolio. The function returns the negative residuals from this model.
#'
#' @param volume A numeric matrix, the volume data.
#' @param returns A numeric matrix, the returns data.
#' @param t An integer, the day for which to perform regression.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc_v An integer, the number of eigen portfolios to extract from the volume data.
#' @param nr_pc An integer, the number of eigen portfolios to extract from the returns data.
#' @param alpha A numeric, the factor to be used in the calculation of overtrade amounts.
#'
#' @return A numeric vector of residuals from the cross-sectional regression model for the day `t`.
#'
#' @examples
#' #Example data
#' volume <- matrix(rnorm(25), 5, 5)
#' returns <- matrix(rnorm(25), 5, 5)
#' t <- 5
#' h <- 4
#' nr_pc_v <- 2
#' nr_pc_r <- 2
#' alpha <- 0.05
#' #Use the function
#' singleCrossSectionalRegressionOvertraded(volume, returns, t, h, nr_pc_v, nr_pc_v, alpha)
#' 
singleCrossSectionalRegressionOvertraded <- function(volume, returns, t, h, nr_pc_v, nr_pc, alpha) {
  standardised_volume <- volume[, (t - h):(t - 1)] / apply(volume[, (t - h):(t - 1)], 1, sum)
  e_volume <- extractEigenPortfolio(standardised_volume, nr_pc_v)

  overtrade_amounts <- sapply(1:h, function(i) {
    model <- lm(standardised_volume[, i] ~ e_volume$portfolio)
    return(model$residuals)
  })

  overtrade_exp_amounts <- exp(alpha * overtrade_amounts)
  weighted_returns <- returns[, (t - h):(t - 1)] / overtrade_exp_amounts

  e <- extractEigenPortfolio(weighted_returns, nr_pc = nr_pc)
  model <- lm(weighted_returns[, H] ~ e$portfolio)
  return(-model$residuals)
}

#' Cross-Sectional Regression for Overtraded Volume
#'
#' This function performs cross-sectional regression on a range of days to predict returns, considering overtraded volume. 
#' For each day in the range, it standardises the volume, calculates overtrade amounts, adjusts the returns by the 
#' exponential of the overtrade amounts, and fits a linear model. The function utilizes parallel computing to increase speed.
#'
#' @param start An integer, the start of the day range for which to perform regression.
#' @param end An integer, the end of the day range for which to perform regression.
#' @param volume A numeric matrix, the volume data.
#' @param returns A numeric matrix, the returns data.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc_v An integer, the number of eigen portfolios to extract from the volume data.
#' @param alpha A numeric, the factor to be used in the calculation of overtrade amounts.
#' @param nr_pc An integer, the number of eigen portfolios to extract from the returns data.
#'
#' @return A numeric matrix of predicted returns where each row corresponds to a day in the given `start:end` range and each column corresponds to a stock.
#'
#' @examples
#' #Example data
#' volume <- matrix(rnorm(25), 5, 5)
#' returns <- matrix(rnorm(25), 5, 5)
#' start <- 2
#' end <- 5
#' h <- 4
#' nr_pc_v <- 2
#' alpha <- 0.05
#' nr_pc <- 2
#' #Use the function
#' crossSectionalRegressionOvertraded(start, end, volume, returns, h, nr_pc_v, alpha, nr_pc)
#' 
crossSectionalRegressionOvertraded <- function(start, end, volume, returns, h, nr_pc_v, alpha, nr_pc) {
  global_vars <- c(
    "singleCrossSectionalRegressionOvertraded", "extractEigenPortfolio",
    "constructEigenPortfolios",
    "constructRho"
  )
  local_vars <- c("volume", "returns", "h", "nr_pc_v", "alpha", "nr_pc")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())


  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegressionOvertraded(
      volume = volume, returns = returns,
      t = t, h = h,
      nr_pc_v = nr_pc_v, nr_pc = nr_pc,
      alpha = alpha
    )
  })

  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}
