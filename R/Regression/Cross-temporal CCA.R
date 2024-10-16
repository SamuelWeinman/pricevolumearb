library(parallel)
library(CCA)

###PERFORM CCA ON THE SPACE OF VOLUMES & RETURNS
###THEN PERFORMS CROSS-TEMPORAL REGRESSION USING THE EMBEDDINGS OF RETURNS AND VOLUMES
#VOLUME SHOULD ALREADy BE STANDARDISED!!
#PARAMETERS AS FOLLOWS:
# T: WHAT DAy WE ARE FORECASTING
# H: NR OF DAyS OF RETURN TO USE FOR EMBEDDING
# HV: NR OF DAyS OF VOLUME TO USE FOR EMBEDDING
# L: NR OF DAyS TO USE FOR REGRESSION ON EIGENRETURNS
# NRC.R: HOW MANy FEAURES OF RETURN EMBEDDINGS TO USE
# NRC.V: HOW MANy FEAURES OF VOLUME EMBEDDINGS TO USE
# b_sensitivity: HOW CLOSE REGRESSION HAS TO BE TO ONE IN ORDER TO REJECT MEAN-REVERSION
singleCrossTemporalRegressionCCA <- function(returns, standardisedVolume,t,h, hv,L, nr_c_r, nr_c_v, b_sensitivity) {

  x <- returns[, (t-h):(t-1)]
  y <- standardisedVolume[, (t-hv):(t-1)]
  
  c <- cc(x, y)
  
  x <- as.matrix(x) %*% as.matrix(c$xcoef)
  y <- as.matrix(y) %*% as.matrix(c$ycoef)
  
  x <- x[, 1:nr_c_r]
  y <- y[, 1:nr_c_r]

  x <- x / apply(as.matrix(returns[,(t-h):(t-1)]),1, sd)
  y <- y / apply(as.matrix(returns[,(t-h):(t-1)]),1, sd)

  portfolios <- cbind(x, y)
  portfolioReturns <- t(returns[(t-L):(t-1)]) %*% portfolios
  
  n <- nrow(returns)
  models <- lapply(1:n, function(i) {
    z <- unlist(returns[i, (t-L):(t-1)])
    model <- lm(as.numeric(z)~portfolioReturns)
    return(model)
  })
  
  coefficients <- estimateCoefficeients(models, b_sensitivity = b_sensitivity)
  
  s <- numeric(nrow(returns))
  index <- coefficients$is_mean_reverting == 1 
  s[index] <- -coefficients$m[index]/sqrt(coefficients$sigma_eq_squared[index])
  
  return(list(
    s = s,
    is_mean_reverting = coefficients$is_mean_reverting))
}


crossTemporalRegressionCCA <- function(returns, Volume, Start, End,H,HV,L, nr_c_r, nr_c_v,d, b_sensitivity) {

  standardisedVolume <- Volume / t(roll_mean(t(as.matrix(Volume)), width = d))

  globalvarlist <- c("DayCrossTemporal.CCA", "estimateCoefficeients")
  localvarlist <- c("returns","H","HV","L", "nr_c_r", "nr_c_v","b_sensitivity", "standardisedVolume")
  
  cl <- snow::makeCluster(detectCores()-1)
  clusterCall(cl, function() library("CCA"))
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir <- environment())
  
  predictions <- snow::parSapply(cl, Start:End, function(t) {
    s <- DayCrossTemporal.CCA(returns = returns, standardisedVolume = standardisedVolume,
                              t=t,H=H, HV = HV,
                              L=L, nr_c_r = nr_c_r, nr_c_v = nr_c_v,
                              b_sensitivity = b_sensitivity)
    p <- -s_scores
    return(p)
  })
  snow::stopCluster(cl)
  
  rownames(Predictions) <- rownames(returns)
  colnames(Predictions) <- Start:End
  
  return(Predictions)
}
