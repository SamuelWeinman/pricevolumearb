#' Single Cross-Sectional Regression with Kernel Canonical Correlation Analysis (KCCA)
#'
#' This function performs cross-sectional regression on scaled returns on a given day,
#' using recent historical data. It uses KCCA to find correlation between the returns 
#' and volume data. Then, a linear model is fit between the returns of the previous day
#' and the obtained KCCA coefficients of returns and volume. The function returns the 
#' negative residuals from this model as predicted returns.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param t An integer, the day for which to perform regression.
#' @param h An integer, number of recent historical days to be used for returns data.
#' @param hv An integer, number of recent historical days to be used for volume data.
#' @param nr_c_r An integer, the number components of returns to be used in KCCA.
#' @param nr_c_v An integer, the number components of volume to be used in KCCA.
#'
#' @return A numeric vector of predictions for returns on day `t`, derived from the regression model using KCCA.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(20), 4, 5)
#' volume <- matrix(rnorm(20), 4, 5)
#' t <- 5
#' h <- 2
#' hv <- 2
#' nr_c_r <- 2
#' nr_c_v <- 2
#' #Use the function
#' singleCrossSectionalRegressionWithKCCA(returns, volume, t, h, hv, nr_c_r, nr_c_v)
#' 
singleCrossSectionalRegressionWithKCCA <- function(returns, volume, t, h, hv, nr_c_r, nr_c_v) {
  x <- as.matrix(returns[, (t - h):(t - 1)])
  y <- as.matrix(volume[, (t - hv):(t - 1)])

  c <- kcca(x, y, ncomps = max(c(nr_c_r, nr_c_v)))

  x_coeff <- c@xcoef[, 1:nr_c_r]
  y_coeff <- c@ycoef[, 1:nr_c_v]

  x_coeff <- x_coeff / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)
  y_coeff <- y_coeff / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)

  x <- x / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)
  y <- y / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)

  model <- lm(returns[, (t - 1)] ~ x + y)

  prediction <- -1 * model$residuals

  return(prediction)
}


#' Cross-Sectional Regression with Kernel Canonical Correlation Analysis (KCCA)
#'
#' This function performs cross-sectional regression on a range of days to predict returns, considering the effect of volume. 
#' For each day in the range, it standardizes the volume, applies KCCA to find correlation between the returns and volume data,
#' and fits a linear model. The residuals from the model are returned as predicted returns. The function is parallelized for increased speed.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param t An integer, the day for which to perform regression.
#' @param h An integer, number of recent historical days to be used for returns data.
#' @param d An integer, the width for rolling mean calculation on volume.
#' @param hv An integer, number of recent historical days to be used for volume data.
#' @param nr_c_r An integer, the number components of returns to be used in KCCA.
#' @param nr_c_v An integer, the number components of volume to be used in KCCA.
#'
#' @return A numeric matrix of predicted returns where each row corresponds to a day in the given `t` range and each column corresponds to a stock.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(20), 4, 5)
#' volume <- matrix(rnorm(20), 4, 5)
#' t <- 5
#' h <- 2
#' d <- 2
#' hv <- 2
#' nr_c_r <- 2
#' nr_c_v <- 2
#' #Use the function
#' crossSectionalRegressionWithKCCA(returns, volume, t, h, d, hv, nr_c_r, nr_c_v)
#' 
crossSectionalRegressionWithKCCA <- function(returns, volume, t, h, d, hv, nr_c_r, nr_c_v) {
  standardised_volume <- volume / t(roll_mean(t(as.matrix(volume)), width = d))

  global_vars <- c("singleCrossSectionalRegressionWithKCCA ", "extractEigenPortfolio", "constructEigenPortfolios", "constructRho")
  local_vars <- c("returns", "h", "hv", "nr_c_r", "nr_c_v", "standardised_volume")


  cl <- snow::makeCluster(parallel::detectCores() - 1)
  clusterCall(cl, function() library("kernlab"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())

  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegressionWithKCCA(
      returns = returns,
      volume = standardised_volume,
      t = t,
      h = h, hv = hv,
      nr_c_r = nr_c_r, nr_c_v = nr_c_v
    )
  })

  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}
