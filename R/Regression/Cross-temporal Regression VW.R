#CALCULATE PREDICTION OF ALL DAYS IN [START, END], USING HISTORICAL DATA
#START: FIRST DAY OF TRADING
#END: LAST DAY OF TRADING
#H: DAYS TO USE TO CONSTRUCT CORRELATION MATRIX
#L: NR OF DAYS TO USE FOR REGRESSION
#D: HOW MANY DAYS TO USE FOR ROLLING MEAN VOLUME
#ALL ELSE AS BEFORE
CTRegression.VW <- function(Returns, Volume, Start, End, nr_pc,H, L, b_sensitivity, d, divide){
  
  #CONSTRUCT WEIGHTED RETURN
  WeightedReturns = ConstructWeightedReturn(Returns = Returns, Volume=Volume,H=H, d = d,
                                            divide = divide)
  
  #PREPARE CORES

  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  Globalvarlist = c("calculateSScore",
                    "estimateCoefficeients","decompose", "ExtractEigenPortfolio",                      
                    "ConstructEigenPortfolios", "ConstructRho", "ConstructWeightedReturn")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  Localvarlist = c("Returns", "H", "L", "b_sensitivity", "WeightedReturns")

  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment()) 

  
  #FOR EACH DAY, FIRST STANDARDISE THE RETURNS AND THEN CALUCLATE THE S-SCORE VECTOR (OVER ALL STOCKS)
  S.Scores = snow::parSapply(cl, Start:End, function(t) {
    S = calculateSScore(Returns = WeightedReturns[, 1:(t-1)], 
                           nr_pc = nr_pc,
                           H = H, L = L,
                           b_sensitivity = b_sensitivity)
    
    #RETURN THE S-SCORE
    return(S$S)
  })

  #STOP CLUSTERS
  snow::stopCluster(cl)
  
  #GET FORECASTS (BASED ON HISTORICAL DATA)
  #THE ROW NAMES WILL THE STOCK TICKERS
  #THE COLUMN NAMES WILL BE THE CORRESPONDING COLUMN NUMBERS IN RETURN
  P = -S.Scores
  rownames(P) = rownames(Returns)
  colnames(P) = Start:End
  
  #RETURN
  return(P)
}

#DOES CrossSectionRegression.VW, BUT AFTER A TRANSFORMATION OF VOLUME THROUGH MAPPING. 
# MAP.list: A LIST OF FUNCTIONS (F1,F2,..., FJ) S.T. VOLUME TRANSFORMED BY VOLUME -> F(VOLUME) BEFORE WEIGHTING.
Outside_CTRegression.VW = function(Returns, Volume, Start, End, nr_pc,H, L, b_sensitivity, d, divide, MAP.list) {
  
  #NR OF MAPS
  K = length(MAP.list)
  
  #CREATE LIST TO STORE PREDICTIONS
  PredictionsList = list()
  
  #FOR EACH MAP, TRANSFORM VOLUME TO MAPPEDVOLUME AND THEN PROCEED WITH CS VW REGRESSION.
  for (k in 1:K) { #loop through mappings
    map = MAP.list[[k]] #extract map
    MappedVolume = map(Volume) #map volumes
    preds=CTRegression.VW(Returns = Returns, #perform calculations with mapped volume
                          Volume = MappedVolume,
                          Start = Start, End = End, 
                          nr_pc=nr_pc,H=H, L = L,
                          b_sensitivity = b_sensitivity,
                          d = d, divide = divide)
    PredictionsList[[k]] = preds #add to list
  }
  #RETURN
  return(PredictionsList)
}
  