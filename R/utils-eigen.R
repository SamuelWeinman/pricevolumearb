#' Construct Correlation Matrix
#'
#' This function constructs a correlation matrix (rho) from a matrix of returns.
#' Each row of the matrix is scaled to have a mean of 0 and standard deviation of 1 
#' before calculating the correlation matrix.
#'
#' @param returns A numeric matrix, the returns data.
#'
#' @return A correlation matrix with row-wise scaled data.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' #Use the function
#' constructRho(ret)
constructRho <- function(returns) {
  y <- apply(returns, 1, scale)
  rho <- cor(y)
  return(rho)
}

#' Construct Eigen Portfolios
#'
#' This function constructs eigen portfolios from a matrix of returns. It first constructs 
#' a correlation matrix (rho) of the returns. Then it computes the eigenvalues and eigenvectors 
#' of the correlation matrix. The eigenvectors are then normalized by the standard deviation 
#' of the returns for each stock. The function also computes the returns for each eigen portfolio.
#'
#' @param returns A numeric matrix, the returns data.
#'
#' @return A list containing:
#' * `rho`: The correlation matrix for the returns.
#' * `portfolios`: The eigen portfolios.
#' * `returns`: The returns of each eigen portfolio.
#' * `values`: The eigenvalues of the correlation matrix.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' #Use the function
#' constructEigenPortfolios(ret)
constructEigenPortfolios <- function(returns) {
  rho <- constructRho(returns)
  e <- eigen(rho)
  sigma_per_stock <- apply(returns, 1, sd)
  eigen_portfolios <- e$vectors / sigma_per_stock
  eigen_returns <- t(eigen_portfolios) %*% as.matrix(returns)
  return(list(
    rho = rho,
    portfolios = eigen_portfolios,
    returns = eigen_returns,
    values = e$values
  ))
}

#' Extract Eigen Portfolios
#'
#' This function extracts a specified number of eigen portfolios from a matrix of returns. 
#' It uses the `constructEigenPortfolios` function to first create all eigen portfolios, 
#' then selects the first `nr_pc` portfolios based on the eigenvalues.
#'
#' @param returns A numeric matrix, the returns data.
#' @param nr_pc An integer, the number of eigen portfolios to extract.
#'
#' @return A list containing:
#' * `rho`: The correlation matrix for the returns.
#' * `portfolio`: The selected eigen portfolios.
#' * `return`: The returns of the selected eigen portfolios.
#' * `prop_explained`: The proportion of variance explained by the selected eigen portfolios.
#'
#' @examples
#' #Example data
#' ret <- matrix(rnorm(25), 5, 5)
#' x <- 2
#' #Use the function
#' extractEigenPortfolios(ret, x)
extractEigenPortfolios <- function(returns, nr_pc) {
  full_eigen_portfolios <- constructEigenPortfolios(returns)
  prop_explained <- sum(full_eigen_portfolios$values[1:nr_pc]) / sum(full_eigen_portfolios$values)
  return(list(
    rho = full_eigen_portfolios$rho,
    portfolio = full_eigen_portfolios$portfolios[, 1:nr_pc],
    return = full_eigen_portfolios$portfolios[1:nr_pc, ],
    prop_explained = sum(full_eigen_portfolios$values[1:nr_pc]) / sum(full_eigen_portfolios$values)
  ))
}
