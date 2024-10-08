library(parallel)


### PERFORMS CT REGRESSION ON ALL DAYS IN INTERVAL [START, END]
#START: FIRST DAY OF TRADING
#END: LAST DAY OF TRADING
#ALL ELSE AS BEFORE
CTRegression <- function(Returns, Start, End, NrPC,H, L, bSensativity) {

  #PREPARE CORES#

  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  Globalvarlist = c("CalculatingS.Score",
                    "EstimateCoefficeients","Decomposition", "ExtractEigenPortfolio",
                    "ConstructEigenPortfolios", "ConstructRho")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  Localvarlist = c("Returns", "H", "L", "bSensativity", "NrPC")

  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment()) 

  
  
  #FOR EACH DAY, CALUCLATE THE S-SCORE VECTOR (OVER ALL STOCKS)
  S.Scores = snow::parSapply(cl, Start:End, function(t) {
    S = CalculatingS.Score(Returns = Returns[, 1:(t-1)], #using only historical data
                           NrPC = NrPC,
                           H = H, L = L,
                           bSensativity = bSensativity)
    
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
  