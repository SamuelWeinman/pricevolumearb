### DECOMPOSE RETURNS INTO TWO PARTS:
# 1. DEPENDING ON THE RETURNS OF EIGENPORTFOLIOS
# 2. INDEPENDENT OF EIGENPORTFOLIOS
#  RETURNS: RETURN MATRIX TO CONSTRUCT RHO (FULL HISTORY UP TO TIME T-1)
#  h (history): NR OF DAYS TO USE TO CONSTRUCT CORRELATION MATRIX
#  l: NR OF DAYS TO USE FOR REGRESSION MODEL (NOT NECESSARILY SAME AS NR OF DAYS FOR CONSTRUCTING RHO, I.E. NCOL(returns_short))
#  nr_pc: AS ABOVE, FOR EIGENdecompose.
decompose <- function(returns, nr_pc, h, L) {
  n <- nrow(returns)
  returns_short <- returns[, (ncol(returns) - h + 1):ncol(returns)] # use last H days
  eigen <- extractEigenPortfolio(returns_short, nr_pc)

  models <- lapply(1:n, function(i) {
    y <- returns_short[i, (h - l + 1):h]
    X <- eigen$return[, (h - l + 1):h]
    model <- lm(as.numeric(y) ~ t(as.matrix(X)))
    return(model)
  })

  return(models)
}


#' Estimate Key Coefficients For All Models
#'
#' This function applies the `estimateSingleCoefficientsNumber` function to all models
#' in the list. After key coefficients are calculated for each model, results are 
#' combined in a single data frame.
#'
#' @param models A list of linear models for each stock, as obtained from the `decompose` function.
#' @param b_sensitivity A numeric value, the sensitivity for b parameter to declare model as mean reverting.
#'
#' @return A data frame where each row corresponds to the estimated key coefficients of each model: 
#' 'k' for speed of mean reversion, 'm' for the long term mean, 'sigma_squared' for variance, 
#' 'sigma_eq_squared' for equilibrium variance and 'is_mean_reverting' for the model's mean-reverting status.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' num_pc <- 2
#' historical <- 4
#' preceding <- 1
#' sensitivity <- 0.1
#' #Use the function
#' models <- decompose(ret, num_pc, historical, preceding)
#' estimateCoefficients(models, sensitivity)
#' 
#' @export
estimateCoefficeients <- function(models, b_sensitivity) {
  coefficients <- lapply(1:length(models), function(i) {
    return(estimateSingleCoefficeientsNumber(models, i, b_sensitivity))
  })

  coefficients <- ldply(coefficients)
  colnames(coefficients) <- c("k", "m", "sigma_squared", "sigma_eq_squared", "is_mean_reverting")
  return(coefficients)
}

#' Estimates Mean Reverting Coefficients
#'
#' This function estimates the key coefficients in a mean reverting model. 
#' A linear model is created first using cumulative sum of residuals as variables. 
#' Then, values for speed of mean reversion (k), long term mean (m), variance (sigma squared),
#' equilibrium variance (sigma_eq_squared) and mean-reverting indicator are calculated.
#'
#' @param models A list of linear models for each stock, as obtained from the `decompose` function.
#' @param i An integer, the index of the specific model required from the list.
#' @param b_sensitivity A numeric value, sensitivity for checking if model_b < 1 - b_sensitivity.
#'
#' @return A numeric vector of length 5 containing key model parameters.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' num_pc <- 2
#' historical <- 4
#' preceding <- 1
#' sensitivity <- 0.1
#' #Use the function
#' models <- decompose(ret, num_pc, historical, preceding)
#' estimateCoefficeientsNumberI(models, 1, sensitivity)
#' 
#' @export
estimateSingleCoefficeientsNumber <- function(models, i, b_sensitivity) {
  x <- cumsum(models[[i]]$residuals)
  model <- lm(x[2:l] ~ x[1:(l - 1)])
  a <- as.numeric(model$coefficients[1])
  b <- as.numeric(model$coefficients[2])
  v <- as.numeric(var(model$residuals))

  k <- -log(b) * 252
  m <- a / (1 - b)
  sigma_squared <- v * 2 * k / (1 - b^2)
  sigma_eq_squared <- v / (1 - b^2)

  is_mean_reverting <- I(b < 1 - b_sensitivity)

  return(c(k, m, sigma_squared, sigma_eq_squared, is_mean_reverting))
}


#' Calculate S-Score for Mean Reverting Stocks
#'
#' This function calculates the S-Score for each stock which is proven to be mean reverting.
#' It first decomposes the returns and estimate key coefficients of each model. Based on 
#' these coefficients and mean reverting condition an S-score is calculated.
#'
#' @param returns A numeric matrix, the returns data.
#' @param nr_pc An integer, the number of eigen portfolios to extract.
#' @param h An integer, the number of recent historical days to be used.
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
#' num_pc <- 2
#' historical <- 4
#' preceding <- 1
#' sensitivity <- 0.1
#' #Use the function
#' calculateSScore(ret, num_pc, historical, preceding, sensitivity)
calculateSScore <- function(returns, nr_pc, h, l, b_sensitivity) {
  models <- decompose(
    returns = returns,
    h = h,
    l = l,
    nr_pc = nr_pc
  )

  coefficients <- estimateCoefficeients(models, b_sensitivity = b_sensitivity)

  s <- numeric(nrow(Returns))
  index <- coefficients$is_mean_reverting == 1 
  s[index] <- -coefficients$m[index] / sqrt(coefficients$sigma_eq_squared[index])

  return(list(
    s = s,
    is_mean_reverting = coefficients$is_mean_reverting
  ))
}
