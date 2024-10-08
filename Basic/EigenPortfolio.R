library(plyr)

###CONSTRUCT CORRELATION MATRIX
#RETURNS: MATRIX OF RETURNS (LAST H DAYS)
ConstructRho <- function(Returns) {
  
  #STANDARDISE
  Y = apply(Returns, 1, scale)
  
  #CONSTRUCT CORRELATION MATRIX
  rho = cor(Y)
  
  #RETURN
  return(rho)
}

###CONSTRUCT EIGENPORTFOLIOS BASED ON RHO MATRIX
#THIS FUNCTION WILL GIVE ALL (N) EIGENPORTFOLIOS
ConstructEigenPortfolios <- function(Returns) {
  
  #CONSTRUCT RHO
  rho = ConstructRho(Returns) 
  
  #GET EIGENVALUES AND VECTORS FROM RHO
  E=eigen(rho) 
  
  #CALCULATE THE SD OF THE RETURNS, BASED ON STOCK
  SigmaPerStock = apply(Returns,1,sd) #the ith entry is the sd of the ith stock
  
  #CONSTRUCT EIGENPORTFOLIOS
  Q = E$vectors / SigmaPerStock 
  
  #CALCULATE THE RETURN OF THE EIGENPORTFOLIOS
  F= t(Q) %*% as.matrix(Returns) 
  
  #RETURN
  return(list(
    rho=rho,
    EigenPortfolio = Q,
    EigenReturn = F,
    EigenValues = E$values
  ))
}
#THE COLUMNS OF Q ARE THE EIGENPORTFOLIOS
#THE ROWS OF F ARE THE RETURNS OF THE EIGENPORTFOLIOS OVER TIME


###GET EIGENPORTFOLIOS, BUT ONLY THE MOST RELEVANT ONES
#  RETURNS: RETURNS MATRIX 
#  NRPC: THE NR OF PC TO USE, I.E. NR OF EIGENPORTFOLIOS TO USE FOR ANALYSIS
ExtractEigenPortfolio <- function(Returns, NrPC) {
  
  #GET FULL EIGEN PORTFOLIO
  EigenPortfolio.Full = ConstructEigenPortfolios(Returns)

  #CALCULATE PROPORTION VARIABILITY EXPLAINED
  PropExplained = sum( EigenPortfolio.Full$EigenValues[1:NrPC] ) / sum(EigenPortfolio.Full$EigenValues) 
  
  
  #RETURN THE RELEVANT EIGENPORTFOLIOS
  return(list(
    rho = EigenPortfolio.Full$rho, 
    EigenPortfolio = EigenPortfolio.Full$EigenPortfolio[,1:NrPC], 
    EigenReturn = EigenPortfolio.Full$EigenReturn[1:NrPC, ], 
    PropExplained = sum( EigenPortfolio.Full$EigenValues[1:NrPC] ) / sum(EigenPortfolio.Full$EigenValues) 
  )) 
}
  
  
  
  
