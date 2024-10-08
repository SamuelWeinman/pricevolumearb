###CONSTRUCT CORRELATION MATRIX
#RETURNS: MATRIX OF RETURNS (LAST H DAYS)
constructRho <- function(returns) {
  
  #STANDARDISE
  y = apply(returns, 1, scale)
  
  #CONSTRUCT CORRELATION MATRIX
  rho = cor(y)
  
  #RETURN
  return(rho)
}

###CONSTRUCT EIGENPORTFOLIOS BASED ON RHO MATRIX
#THIS FUNCTION WILL GIVE ALL (N) EIGENPORTFOLIOS
constructEigenPortfolios <- function(returns) {
  
  #CONSTRUCT RHO
  rho <- constructRho(returns) 
  
  #GET EIGENVALUES AND VECTORS FROM RHO
  e <- eigen(rho) 
  
  #CALCULATE THE SD OF THE RETURNS, BASED ON STOCK
  sigma_per_stock <- apply(returns, 1, sd) #the ith entry is the sd of the ith stock
  
  #CONSTRUCT EIGENPORTFOLIOS
  q <- e$vectors / sigma_per_stock 
  
  #CALCULATE THE RETURN OF THE EIGENPORTFOLIOS
  f <- t(q) %*% as.matrix(returns) 
  
  #RETURN
  return(list(
    rho = rho,
    portfolios = q,
    returns = f,
    values = e$values
  ))
}
#THE COLUMNS OF Q ARE THE EIGENPORTFOLIOS
#THE ROWS OF F ARE THE RETURNS OF THE EIGENPORTFOLIOS OVER TIME


###GET EIGENPORTFOLIOS, BUT ONLY THE MOST RELEVANT ONES
#  RETURNS: RETURNS MATRIX 
#  nr_pc: THE NR OF PC TO USE, I.E. NR OF EIGENPORTFOLIOS TO USE FOR ANALYSIS
extractEigenPortfolios <- function(returns, nr_pc) {
  
  #GET FULL EIGEN PORTFOLIO
  full_eigen_portfolios <- constructEigenPortfolios(returns)

  #CALCULATE PROPORTION VARIABILITY EXPLAINED
  prop_explained <- sum(full_eigen_portfolios$values[1:nr_pc]) / sum(full_eigen_portfolios$values) 
  
  
  #RETURN THE RELEVANT EIGENPORTFOLIOS
  return(list(
    rho = full_eigen_portfolios$rho, 
    portfolio = full_eigen_portfolios$portfolios[,1:nr_pc], 
    return = full_eigen_portfolios$portfolios[1:nr_pc, ], 
    prop_explained = sum(full_eigen_portfolios$values[1:nr_pc] ) / sum(full_eigen_portfolios$values) 
  )) 
}
  
  
  
  
