#' Construct Weighted Returns
#'
#' This function generates weighted returns based on the ratio of rolling mean volume to actual volume, 
#' or vice-versa depending on the value of the divide parameter.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param d An integer, the width for calculating the rolling mean of the volume.
#' @param divide A logical value, if TRUE, the function divides the product of returns and rolling mean volume by the volume. If FALSE, it divides the product of returns and volume by the rolling mean volume.
#'
#' @return A matrix of the same dimensions as returns and volume, where each entry corresponds to the weighted return of a stock on a particular day.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' vol <- matrix(rnorm(25), 5, 5)
#' d <- 2
#' divide <- TRUE
#' #Use the function
#' constructWeightedReturns(ret, vol, d, divide)
constructWeightedReturns <- function(returns, volume, d, divide) {
  rolling_mean_volume <- t(roll_mean(t(as.matrix(volume)), width = d))
  res <- ifelse(divide, returns * rolling_mean_volume / volume, returns * volume / rolling_mean_volume)
  return(res)
}

#' Cross-Sectional Regression with Volume Weighting
#'
#' This function performs cross-sectional regression on volume-weighted returns on a range of days,
#' using recent historical data. An eigenportfolio is first extracted from the historical data.
#' Then, a linear model is fit between the returns and the eigenportfolio for each day in the range.
#' The function returns the negative residuals from these models as predicted returns. 
#' This is a parallelized function that takes advantage of multiple cores for speed.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param start An integer, the start of the days range for which to perform regression.
#' @param end An integer, the end of the days range for which to perform regression.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc An integer, the number of eigen portfolios to extract.
#' @param d An integer, the width for rolling mean calculation on volume.
#' @param divide A logical value, if TRUE, the function divides the product of returns and rolling mean volume by the volume. If FALSE, it divides the product of returns and volume by the rolling mean volume.
#'
#' @return A numeric matrix of predicted returns where each row corresponds to a day in the given `start:end` range and each column corresponds to a stock.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' vol <- matrix(rnorm(25), 5, 5)
#' start_range <- 2
#' end_range <- 5
#' historical <- 4
#' num_pc <- 2
#' rolling_mean_width <- 2
#' divide_flag <- TRUE
#' #Use the function
#' crossSectionalRegressionVW(ret, vol, start_range, end_range, historical, num_pc, rolling_mean_width, divide_flag)
crossSectionalRegressionVW <- function(returns, volume, start, end, h, nr_pc, d, divide) {
  weighted_returns <- constructWeightedReturns(
    returns = returns, volume = volume, d = d,
    divide = divide
  )

  global_vars <- c(
    "singleCrossSectionalRegression", "constructWeightedReturns",
    "extractEigenPortfolio", "constructEigenPortfolios",
    "constructRho"
  )

  local_vars <- c("weighted_returns", "h", "nr_pc")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  parallel::clusterCall(cl, function() library("roll"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())


  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegression(
      returns = weighted_returns,
      t = t, h = h,
      nr_pc = nr_pc
    )
  })

  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}



#' Cross-Sectional Regression with Volume Weighting for Mapped Volume
#'
#' This function performs cross-sectional regression on volume-weighted returns on a range of days,
#' using recent historical data and a list of functions to map the volume data. For each mapping function
#' in `map_list`, it calculates the volume-weighted returns, extracts an eigenportfolio from the historical data,
#' fits a linear model between the returns and the eigenportfolio for each day in the range, and returns the negative residuals
#' from these models as predicted returns.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param start An integer, the start of the days range for which to perform regression.
#' @param end An integer, the end of the days range for which to perform regression.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc An integer, the number of eigen portfolios to extract.
#' @param d An integer, the width for rolling mean calculation on volume.
#' @param divide A logical value, if TRUE, the function divides the product of returns and rolling mean volume by the volume. If FALSE, it divides the product of returns and volume by the rolling mean volume.
#' @param map_list A list of functions to be applied on volume data.
#'
#' @return A list of numeric matrices of predicted returns where each element corresponds to map function in `map_list`, and each matrix where a row corresponds to a day in the given `start:end` range and each column corresponds to a stock.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(25), 5, 5)
#' volume <- matrix(rnorm(25), 5, 5)
#' start <- 2
#' end <- 5
#' h <- 4
#' nr_pc <- 2
#' d <- 2
#' divide <- TRUE
#' maps <- list(function(x) x^2, function(x) log2(x+1))
#' #Use the function
#' crossSectionalRegressionMappedVW(returns, volume, start, end, h, nr_pc, d, divide, maps)
#' 
crossSectionalRegressionMappedVW <- function(returns, volume, start, end, h, nr_pc, d, divide, maps) {
  predictions <- list()
  for (k in 1:length(maps)) {
    map <- maps[[k]]
    transformed_volume <- map(volume)
    preds <- crossSectionalRegressionVW(
      returns = returns,
      volume = transformed_volume,
      start = start, end = end,
      h = h,
      nr_pc = nr_pc,
      d = d,
      divide = divide
    )
    predictions[[k]] <- preds
  }

  # RETURN
  return(predictions)
}
