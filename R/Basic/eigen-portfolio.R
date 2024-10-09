constructRho <- function(returns) {
  y <- apply(returns, 1, scale)
  rho <- cor(y)
  return(rho)
}

#CONSTRUCTS EIGENPORTFOLIOS BASED ON RHO
#THIS FUNCTION WILL GIVE ALL (N) EIGENPORTFOLIOS
#THE COLUMNS OF Q ARE THE EIGENPORTFOLIOS
#THE ROWS OF F ARE THE RETURNS OF THE EIGENPORTFOLIOS OVER TIME
constructEigenPortfolios <- function(returns) {
  
  rho <- constructRho(returns) 
  e <- eigen(rho) 
  sigma_per_stock <- apply(returns, 1, sd)
  eigen_portfolios <- e$vectors / sigma_per_stock 
  eigen_returns <- t(eigen_portfolios) %*% as.matrix(returns) 
  return(list(
    rho = rho,
    portfolios = eigen_portfoliosq,
    returns = eigen_returns,
    values = e$values
  ))
}


###GET EIGENPORTFOLIOS, BUT ONLY THE MOST RELEVANT ONES
#  RETURNS: RETURNS MATRIX 
#  nr_pc: THE NR OF PC TO USE, I.E. NR OF EIGENPORTFOLIOS TO USE FOR ANALYSIS
extractEigenPortfolios <- function(returns, nr_pc) {
  
  full_eigen_portfolios <- constructEigenPortfolios(returns)
  prop_explained <- sum(full_eigen_portfolios$values[1:nr_pc]) / sum(full_eigen_portfolios$values) 
  return(list(
    rho = full_eigen_portfolios$rho, 
    portfolio = full_eigen_portfolios$portfolios[, 1:nr_pc], 
    return = full_eigen_portfolios$portfolios[1:nr_pc, ], 
    prop_explained = sum(full_eigen_portfolios$values[1:nr_pc] ) / sum(full_eigen_portfolios$values) 
  )) 
}
  
  
  
  
