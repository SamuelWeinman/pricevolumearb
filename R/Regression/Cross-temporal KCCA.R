library(kernlab)


###PERFORM KCCA ON THE SPACE OF VOLUMES & RETURNS
###THEN PERFORMS CROSS-TEMPORAL REGRESSION USING THE EMBEDDINGS OF RETURNS AND VOLUMES
#VOLUME SHOULD ALREADY BE STANDARDISED!!
#PARAMETERS AS FOLLOWS:
# T: WHAT DAY WE ARE FORECASTING
# H: NR OF DAYS OF RETURN TO USE FOR EMBEDDING
# HV: NR OF DAYS OF VOLUME TO USE FOR EMBEDDING
# L: NR OF DAYS TO USE FOR REGRESSION ON EIGENRETURNS
# NRC.R: HOW MANY FEAURES OF RETURN EMBEDDINGS TO USE
# NRC.V: HOW MANY FEAURES OF VOLUME EMBEDDINGS TO USE
# bSensativity: HOW CLOSE REGRESSION HAS TO BE TO ONE IN ORDER TO REJECT MEAN-REVERSION

DayCrossTemporal.KCCA = function(Returns, Volume,t,H,HV,L,NrC.R, NrC.V,bSensativity) {
  
  #EXTRACT LAST H DAYS OF RETURN HISTORY, GIVING X
  X = as.matrix(Returns[,(t-H):(t-1)])
  
  #EXTEACT VOLUME
  Y = as.matrix(Volume[,(t-HV):(t-1)])

  #PERFORM KCCA
  C = kcca(X,Y, ncomps = max(c(NrC.R,NrC.V)))
  
  #EMBEDDING OF X AND Y IN NEW SPACE
  X = C@xcoef
  Y= C@ycoef
  
  #EXTRACT MOST IMPORTANT COMPONENTS
  X = X[,1:NrC.R]
  Y = Y[,1:NrC.V]
  
  #DIVIDE EACH COLUMN BY THE STANDARD DEVIATION OF THE ROW 
  X = X / apply(as.matrix(Returns[,(t-H):(t-1)]),1,sd)
  Y = Y / apply(as.matrix(Returns[,(t-H):(t-1)]),1,sd)

  #CONSTRUCT "EIGEN"-PORTFOLIO
  Portfolios = cbind(X,Y)
  
  #CALCULATE RETURN ON EACH OF THE PORTFOLIOS
  PortfolioReturns = t(Returns[(t-L):(t-1)]) %*% Portfolios

  #PERFORM REGRESSION
  #THE i:TH ENTRY IN MODELS IS THE REGRESSION OF THE i:TH STOCK ON THE RETURNS OF THE "EIGEN"-PORTFOLIO
  N = nrow(Returns)
  Models <- lapply(1:N, function(i) { #loop through all stocks
    y = unlist(Returns[i,(t-L):(t-1)]) #returns last L days
    model = lm(as.numeric(y)~PortfolioReturns) #get lm
    return(model)
  })
  
  
  #ESTIMATE COEFFICIENTS FROM ABOVE MODELS
  Coefficients = EstimateCoefficeients(Models, bSensativity = bSensativity)
  
  #GET S-SCORE
  S = numeric(nrow(Returns))
  index = Coefficients$MeanReversion==1 #mean reversion 
  S[index] = -Coefficients$m[index]/sqrt(Coefficients$SigmaEq.Squared[index])
  
  #RETURN
  return(list(
    S= S,
    MeanReversion = Coefficients$MeanReversion))
}

#PERFORMS CT KCCA ON [START, END]
#D: HOW MANY DAYS TO STANDARDISE VOLUME OVER
CTRegression.KCCA = function(Returns, Volume,Start, End,H,HV,L,NrC.R, NrC.V,d,bSensativity) {

  #STANDRDISE VOLUME
  #HERE, WE DIVIDE VOLUME BY THE AVERAGE VOLUME DURING THE LAST D DAYS (INCLUDING TODAY)
  StandardisedVolume = Volume / t(roll_mean(t(as.matrix(Volume)), width = d))
  
  #PREPARE CORES#
  
  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  Globalvarlist = c("DayCrossTemporal.KCCA","EstimateCoefficeients")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  Localvarlist = c("Returns","H","HV","L","NrC.R", "NrC.V","bSensativity","StandardisedVolume")
  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  clusterCall(cl, function() library("kernlab"))
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment())
  
  #FOR EACH DAY, CALUCLATE THE S-SCORE VECTOR (OVER ALL STOCKS)
  Predictions = snow::parSapply(cl, Start:End, function(t) {
    S = DayCrossTemporal.KCCA(Returns = Returns, Volume = StandardisedVolume,
                             t=t,H=H,HV = HV,
                             L=L,NrC.R = NrC.R, NrC.V = NrC.V,
                             bSensativity = bSensativity) #S-score for the day (accross stocks)
    P = -S$S #prediction is negative s-score
    return(P)
  })
  
  #STOP CLUSTERS
  snow::stopCluster(cl)
  
  #CHANGE COL AND ROWNAMES AS APPROPRIATE.
  rownames(Predictions) = rownames(Returns)
  colnames(Predictions) = Start:End
  
  #RETURN
  return(Predictions)
}

