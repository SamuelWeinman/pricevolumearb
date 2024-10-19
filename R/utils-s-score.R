
###DECOMPOSE RETURNS INTO TWO PARTS:
# 1. DEPENDING ON THE RETURNS OF EIGENPORTFOLIOS
# 2. INDEPENDENT OF EIGENPORTFOLIOS
#  RETURNS: RETURN MATRIX TO CONSTRUCT RHO (FULL HISTORY UP TO TIME T-1)
#  h (history): NR OF DAYS TO USE TO CONSTRUCT CORRELATION MATRIX
#  l: NR OF DAYS TO USE FOR REGRESSION MODEL (NOT NECESSARILY SAME AS NR OF DAYS FOR CONSTRUCTING RHO, I.E. NCOL(returns_short))
#  nr_pc: AS ABOVE, FOR EIGENdecompose.
decompose <- function(returns, nr_pc, h, L) {
  n <- nrow(returns)
  returns_short <- returns[ ,(ncol(returns)-h+1) : ncol(returns)] #use last H days
  eigen <- extractEigenPortfolio(returns_short, nr_pc)

  models <- lapply(1:n, function(i) { #loop through all stocks
    y = returns_short[i, (h-l+1):h] #returns last L days
    X = eigen$return[, (h-l+1):h] #eigenreturns last L days
    model = lm(as.numeric(y)~t(as.matrix(X))) #get lm
    return(model)
  })
  
  #RETURN MODELS
  print(paste("Variability explained:", eigen$prop_explained))
  return(models)
}


###ESTIMATE THE COEFFICIENTS OF THE UHRNBECK MODEL, FOR EACH STOCK SEPRATELY
#MODELS: MODELS OF THE FORM (RETURNS ~ FACTORS)
#FOLLOWS SAME ALGORITHM AS APPENDIX A OF Marco Avellaneda & Jeong-Hyun Lee (2010)
#IF B CLOSE TOO ONE, THERE IS NO MEAN REVERSION
#HENCE, REJECT MEAN REVERSION IF b > 1-b_sensitivity
estimateCoefficeients <- function(models, b_sensitivity) {
  
  coefficients <- lapply(1:length(models), function(i){
    return(estimateCoefficeientsNumberI(models, i, b_sensitivity))
  })
  
  #UPON CALCULATING COEFFICIENTS FOR EACH MODEL, PUT IN DATA FRAME
  coefficients <- ldply(coefficients) #DATA FRAME
  colnames(coefficients) <- c("k", "m", "sigma_squared", "sigma_eq_squared", "is_mean_reverting")
  #RETURN
  return(coefficients)
}

estimateCoefficeientsNumberI <- function(models, i, b_sensitivity) {
  x <- cumsum(models[[i]]$residuals)
  model <- lm(x[2:l] ~ x[1:(l-1)])
  a <- as.numeric(model$coefficients[1]) 
  b <- as.numeric(model$coefficients[2])
  v <- as.numeric(var(model$residuals))

  k <- -log(b)*252
  m <- a/(1-b)
  sigma_squared <- v * 2*k / (1-b^2)
  sigma_eq_squared <- v/(1-b^2)
  
  is_mean_reverting <- I(b < 1-b_sensitivity)
  
  return(c(k, m, sigma_squared, sigma_eq_squared, is_mean_reverting))
}


###CALCULATE S-SCORE FOR ALL STOCKS AS IN Marco Avellaneda & Jeong-Hyun Lee (2010)
###THAT IS, THE AMOUNT BY WHICH THE STOCKS HAVE HAD TOO MUCH RETURNS
###IF MEAN-REVERSION == 0, WE GIVE s = 0 AS THERE IS NO MEAN REVERSION.
#RETURNS MUST BE HISTORY ONLY!
calculateSScore <- function(returns, nr_pc, h, l, b_sensitivity) {

  models <- decompose(
    returns = returns,
    h = h,
    l = l,
    nr_pc = nr_pc
  )

  coefficients = estimateCoefficeients(models, b_sensitivity = b_sensitivity)
  
  s = numeric(nrow(Returns))
  index = coefficients$is_mean_reverting == 1 #mean reversion 
  s[index] = -coefficients$m[index]/sqrt(coefficients$sigma_eq_squared[index]) #CORRESPONDING S-SCORE
  
  return(list(
  s = s,
  is_mean_reverting = coefficients$is_mean_reverting))
}