#' Single Cross-Sectional Regression
#'
#' This function performs cross-sectional regression on returns on a given day,
#' using recent historical data. An eigenportfolio is first extracted from the historical data.
#' Then, a linear model is fit between the returns on the day and the eigenportfolio.
#' The function returns the negative residuals from this model which denote the predicted returns.
#'
#' @param returns A numeric matrix, the returns data.
#' @param t An integer, the day for which to perform regression.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc An integer, the number of eigen portfolios to extract.
#'
#' @return A numeric vector of predictions for returns on day `t`, derived from the cross-sectional regression model.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(20), 4, 5)
#' t_val <- 5
#' historical <- 4
#' num_pc <- 2
#' #Use the function
#' singleCrossSectionalRegression(ret, t_val, historical, num_pc)
singleCrossSectionalRegression <- function(returns, t, h, nr_pc) {
  e <- extractEigenPortfolio(
    returns = returns[, (t - h):(t - 1)],
    nr_pc = nr_pc
  )

  model <- lm(returns[, t - 1] ~ e$portfolio)
  p <- -model$residuals
  return(p)
}



#' Cross-Sectional Regression
#'
#' This function performs cross-sectional regression on returns on a range of days,
#' using recent historical data. An eigenportfolio is first extracted from the historical data.
#' Then, a linear model is fit between the returns and the eigenportfolio for each day in the range.
#' The function returns the negative residuals from these models as predicted returns. 
#' This function utilizes parallel computing for efficiency.
#'
#' @param returns A numeric matrix, the returns data.
#' @param start An integer, the start of the days range for which to perform regression.
#' @param end An integer, the end of the days range for which to perform regression.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc An integer, the number of eigen portfolios to extract.
#'
#' @return A numeric matrix of predicted returns where each row corresponds to a day in the given `start:end` range and each column corresponds to a stock.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' start_range <- 2
#' end_range <- 5
#' historical <- 4
#' num_pc <- 2
#' #Use the function
#' crossSectionalRegression(ret, start_range, end_range, historical, num_pc)
crossSectionalRegression <- function(returns, start, end, h, nr_pc) {
  global_vars <- c(
    "singleCrossSectionalRegression",
    "extractEigenPortfolio", "constructEigenPortfolios",
    "constructRho"
  )

  local_vars <- c("returns", "h", "nr_pc")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())

  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegression(
      returns = returns,
      t = t, h = h,
      nr_pc = nr_pc
    )
  })
  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}
