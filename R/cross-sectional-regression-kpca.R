#' Single Cross-Sectional Regression with Kernel Principal Component Analysis (KPCA)
#'
#' This function performs cross-sectional regression on scaled returns on a given day,
#' using recent historical data. KPCA is applied to the scaled returns. Then, a linear 
#' model is fit between the last day's returns and the KPCA rotated returns. 
#' The function returns the negative residuals from this model which denote the predicted returns.
#'
#' @param returns A numeric matrix, the returns data.
#' @param t An integer, the day for which to perform regression.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc An integer, the number of principal components to keep in KPCA.
#' @param kernel A character string, the kernel to be used in KPCA.
#' @param kpar A list, the list of hyperparameters for the kernel.
#'
#' @return A numeric vector of predictions for returns on day `t`, derived from the regression model using KPCA.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(20), 4, 5)
#' t <- 5
#' h <- 4
#' nr_pc <- 2
#' kernel <- "rbfdot"
#' kpar <- list(sigma = 0.1)
#' #Use the function
#' singleCrossSectionalRegressionKPCA(returns, t, h, nr_pc, kernel, kpar
singleCrossSectionalRegressionKPCA <- function(returns, t, h, nr_pc, kernel, kpar) {
  returns <- returns[, (t - h):(t - 1)]
  returns <- apply(returns, 2, scale)

  s <- kpca(returns,
    features = nr_pc,
    kernel = kernel, kpar = kpar
  )

  x <- x@rotated / apply(returns, 1, sd)
  y <- returns[, ncol(returns)] # last days of returns
  model <- lm(y ~ X)
  pred <- -model$residuals

  return(pred)
}


#' Cross-Sectional Regression with Kernel Principal Component Analysis (KPCA)
#'
#' This function performs cross-sectional regression on a range of days, using recent historical data. 
#' The analysis uses Kernel Principal Component Analysis (KPCA) with the provided kernel and its parameters. 
#' The parallelized function applies Kernel PCA to the scaled returns to extract principal components, then fits a 
#' linear model and calculates residuals as predicted returns.
#'
#' @param returns A numeric matrix, the returns data.
#' @param start An integer, the start of the days range for which to perform regression.
#' @param end An integer, the end of the days range for which to perform regression.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc An integer, the number of principal components to use in KPCA.
#' @param kernel A character string, the kernel to be used in KPCA.
#' @param kpar A list, the list of hyperparameters for the kernel.
#'
#' @return A numeric matrix of predicted returns where each row corresponds to a day in the given `start:end` range and each column corresponds to a stock.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(20), 4, 5)
#' start <- 1
#' end <- 4
#' h <- 2
#' nr_pc <- 2
#' kernel <- "rbfdot"
#' kpar <- list(sigma = 0.1)
#' #Use the function
#' crossSectionalRegressionKPCA(returns, start, end, h, nr_pc, kernel, kpar)
#' 
crossSectionalRegressionKPCA <- function(returns, start, end, h, nr_pc, kernel, kpar) {
  global_vars <- c("singleCrossSectionalRegressionKPCA", "constructRho")
  local_vars <- c("returns", "h", "nr_pc", "kernel", "kpar")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  clusterCall(cl, function() library("kernlab"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())


  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegressionKPCA(
      returns = returns,
      t = t,
      h = h, nr_pc = nr_pc,
      kernel = kernel, kpar = kpar
    )
  })

  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}
