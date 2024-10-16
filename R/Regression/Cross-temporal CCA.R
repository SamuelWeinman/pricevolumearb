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

DayCrossTemporal.CCA = function(returns, StandardVolume,t,H,HV,L, NrC.R, NrC.V, b_sensitivity) {

  #ExTRACT LAST H DAyS OF RETURN HISTORy, GIVING x
  x <- returns[, (t-H):(t-1)]

  #ExTEACT VOLUME
  y <- StandardVolume[, (t-HV):(t-1)]
  
  #PERFORM CCA
  c = cc(x, y)
  
  #EMBEDDING OF x AND y IN NEW SPACE
  x <- as.matrix(x) %*% as.matrix(C$xcoef)
  y <- as.matrix(y) %*% as.matrix(C$ycoef)
  
  #ExTRACT MOST IMPORTANT COMPONENTS
  x <- x[, 1:NrC.R]
  y <- y[, 1:NrC.V]

  #DIVIDE EACH COLUMN By THE STANDARD DEVIATION OF THE ROW 
  x <- x / apply(as.matrix(returns[,(t-H):(t-1)]),1, sd)
  y <- y / apply(as.matrix(returns[,(t-H):(t-1)]),1, sd)

  #CONSTRUCT "EIGEN"-PORTFOLIO
  portfolios = cbind(x, y)
  
  #CALCULATE RETURN ON EACH OF THE PORTFOLIOS
  portfolioReturns = t(returns[(t-L):(t-1)]) %*% portfolios
  
  #PERFORM REGRESSION
  #THE i:TH ENTRy IN MODELS IS THE REGRESSION OF THE i:TH STOCK ON THE RETURNS OF THE "EIGEN"-PORTFOLIO
  N = nrow(returns)
  models <- lapply(1:N, function(i) { #loop through all stocks
    y <- unlist(returns[i, (t-L):(t-1)]) #returns last L days
    model = lm(as.numeric(y)~portfolioReturns) #get lm
    return(model)
  })
  
  #ESTIMATE COEFFICIENTS FROM ABOVE MODELS
  coefficients = estimateCoefficeients(models, b_sensitivity = b_sensitivity)
  
  #GET S-SCORE
  s = numeric(nrow(returns))
  index <- coefficients$MeanReversion == 1 #mean reversion 
  s[index] = -coefficients$m[index]/sqrt(coefficients$SigmaEq.Squared[index])
  
  #RETURN
  return(list(
    s = s,
    MeanReversion = coefficients$MeanReversion))
}


#PERFORMS CT CCA ON [START, END]
#D: HOW MANy DAyS TO STANDARDISE VOLUME OVER!!!
CTRegression.CCA = function(returns, Volume, Start, End,H,HV,L, NrC.R, NrC.V,d, b_sensitivity) {

  #STANDRDISE VOLUME
  #HERE, WE DIVIDE VOLUME By THE AVERAGE VOLUME DURING THE LAST D DAyS (INCLUDING TODAy)
  standardisedVolume = Volume / t(roll_mean(t(as.matrix(Volume)), width = d))

  #PREPARE CORES#
  
  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  globalvarlist = c("DayCrossTemporal.CCA", "estimateCoefficeients")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  localvarlist = c("returns","H","HV","L", "NrC.R", "NrC.V","b_sensitivity", "standardisedVolume")
  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  clusterCall(cl, function() library("CCA"))
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment())
  
  #FOR EACH DAy, CALUCLATE THE S-SCORE VECTOR (OVER ALL STOCKS)
  predictions = snow::parSapply(cl, Start:End, function(t) {
    s = DayCrossTemporal.CCA(returns = returns, StandardVolume = standardisedVolume,
                              t=t,H=H, HV = HV,
                              L=L, NrC.R = NrC.R, NrC.V = NrC.V,
                              b_sensitivity <- b_sensitivity)#s-score for the day (accross stocks)
    p = -s_scores #predictions is negative s-score
    return(p)
  })
  
  #STOP CLUSTERS
  snow::stopCluster(cl)
  
  #CHANGE COL AND ROWNAMES AS APPROPRIATE.
  rownames(Predictions) = rownames(returns)
  colnames(Predictions) = Start:End
  
  #RETURN
  return(Predictions)
}
