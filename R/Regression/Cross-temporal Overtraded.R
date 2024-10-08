
#FOR EACH DAY IN THE HISTORY (H), DECOMPOSES THE VOLUMES IN "EIGENVOLUME" PORTFOLIOS, YIELDING RESIDUALS ("OVERTRADED")
#RESIDUALS ARE THEN MAPPED BY RESIDUAL -> EXP(ALPHA * RESIDUALS), AND RETURNS ARE THEN DIVIDED BY THIS AMOUNT.
#THEN PROCEEDS AS NORMAL WITH CROSS TEMPORA; REGRESSION.

#NrPC.V: HOW MANY PC TO USE WHEN CONSTRUCTED "EIGENVOLUME-PORTFOLIOS", 
#I.E. WHENS STUDYING IF (STOCK, DAY) IS OVERTRADED.
# ALPHA: SCALING OF VOLUME RESIDUALS BEFORE TAKING EXPONENTIAL

#NOTE: VOLUMES ARE DECOMPOSED USING CS REGRESSION, ONLY RETURNS ARE DECOMPOSED WITH CROSS-TEMPORAL REGRESSION
Day.CT.Overtrade = function(Volume, Returns, t, H, NrPC.V, NrPC, alpha,L, bSensativity) {
  
  #STANDARDISE VOLUME: DISTRIBUTION OF VOLUME ON THE PRVIOUS H DAYS. I.E. NOT A ROLLING WINDOW APPROACH AS BEFORE! 
  #FOR ANY T, WILL COMPARE ALL THE HISTORY TO THE H DAYS BEFORE T.  
  StandardVolume = Volume[,(t-H):(t-1)]/apply(Volume[,(t-H):(t-1)],1, sum)
  
  #CONSTRUCT "EIGENPORTFOLIO" OF VOLUME
  E.V = ExtractEigenPortfolio(StandardVolume, NrPC.V)
  
  #CALCULATE BY HOW MUCH IT'S STOCK IS OVERTRADED OVERTRADING
  #HERE Overtraded_{I, T} IS THE AMOUNT THAT STOCK I WAS OVERTRADED ON DAY T
  Overtraded = sapply(1:H, function(i) {
    y = StandardVolume[, i] #standardise volume on the day
    model = lm(y~E.V$EigenPortfolio) #regress on eigenportfolios
    return(model$residuals) #extract residuals
  })
  
  #EXPONENTIAL RESIDUALS
  Overtraded= exp(alpha*Overtraded)
  
  #CONSTRUCT WEIGHTED RETURN, PENALISING LARGE TRADING DAYS
  WeightedReturn = Returns[, (t-H):(t-1)]/Overtraded
  
  ###NOW DECOMPOSE RETURNS (CT)####
  
  #REGRESS EACH OF THE STOCKS ON RETURNS OF EIGENPORTFOLIOS
  Models=Decomposition(Returns = WeightedReturn,
                       H = H,
                       L = L,
                       NrPC = NrPC)
  
  #ESTIMATE PARAMETERS
  Coefficients = EstimateCoefficeients(Models, bSensativity)
  
  
  #CALCULATE S-SCORE (NEGATIVE PREDICTION)
  S = numeric(nrow(Returns))
  index = Coefficients$MeanReversion == 1 #mean reversion 
  S[index] = -Coefficients$m[index]/sqrt(Coefficients$SigmaEq.Squared[index])  
  
  #RETURN
  return(list(
    S= S,
    MeanReversion = Coefficients$MeanReversion))
}


###PERFORMS CT OVERTRADED ON INTERVAL [START, END]
#START: FIRST DAY OF TRADING
#END: LAST DAY OF TRADING
#ALL ELSE AS BEFORE
CTRegression.Overtrade <- function(Volume, Returns, Start, End, H, NrPC.V, NrPC, alpha,L, bSensativity) {
  
  #PREPARE CORES#
  
  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  Globalvarlist = c("Day.CT.Overtrade",
                    "EstimateCoefficeients","Decomposition", "ExtractEigenPortfolio",  
                    "ConstructEigenPortfolios", "ConstructRho")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  Localvarlist = c("Returns", "Volume",
                   "H", "L", "bSensativity",
                   "NrPC.V", "NrPC", "alpha","L", "bSensativity")
  
  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment()) 
  
  
  
  #FOR EACH DAY, CALUCLATE THE S-SCORE VECTOR (OVER ALL STOCKS)
  Predictions = snow::parSapply(cl, Start:End, function(t) {
    S = Day.CT.Overtrade(Volume = Volume, Returns = Returns, t = t,
                         H = H, NrPC.V = NrPC.V, NrPC = NrPC,
                         alpha=alpha,L=L, bSensativity = bSensativity) # S-SCORE
    
    P = -S$S #PREDICTION IS NEGATIVE S-SCORE
    return(P)
  })
  
  #STOP CLUSTERS
  snow::stopCluster(cl)
  
  #GET FORECASTS (BASED ON HISTORICAL DATA)
  #THE ROW NAMES WILL THE STOCK TICKERS
  #THE COLUMN NAMES WILL BE THE CORRESPONDING COLUMN NUMBERS IN RETURN
  rownames(Predictions) = rownames(Returns)
  colnames(Predictions) = Start:End
  
  #RETURN
  return(Predictions)
}

  



