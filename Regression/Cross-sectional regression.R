library(snow)
library(parallel)


  

#PERFORM CROSS SECTIONAL REGRESSION ON A PARTICULAR DAY
#  t: column number we are going to trade at
#  H: NR OF DAYS TO USE TO CONSTRUCT CORRELATION MATRIX
#  NrPC: NUMBER OF PC'S TO USE FOR THE REGRESSION
DayCrossRegression <- function(Returns,t,H,NrPC) {
  
  #EXTRACT EIGENPORTFOLIO USING STANDARD PCS
  #USE H DAYS OF HISTORY UP TO TIME t
  E = ExtractEigenPortfolio(Returns=Returns[,(t-H):(t-1)], 
                              NrPC=NrPC) 
  
  #PERFORM REGRESSION AND EXTRACT PREDICTION
  y = Returns[,t-1] #returns on last day
  X= E$EigenPortfolio #the eigenportfolios
  model = lm(y~X) #regression
  P = -model$residuals #prediction 
  
  #RETURN THE PREDICTION
  return(P)
}



#CALCULATE PREDICTION OF ALL DAYS IN [START,END], USING HISTORICAL DATA
#START: FIRST DAY OF TRADING
#END: LAST DAY OF TRADING
CrossSectionRegression <- function(Returns, Start, End, H, NrPC) {

  #PREPARE CORES#
  
  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  Globalvarlist = c("DayCrossRegression",
                    "ExtractEigenPortfolio", "ConstructEigenPortfolios", 
                    "ConstructRho")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  Localvarlist = c("Returns","H","NrPC")
  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment()) 

  #GET PREDICTION OVER THE WHOLE TIME PERIOD
  #ROWS CORRESPOND TO STOCKS
  #THE COLUMNS CORRESPOND TO DAYS IN [START:END]
  Predictions=snow::parSapply(cl, Start:End, function(t) {
    DayCrossRegression(Returns=Returns,
                       t=t,H=H,
                       NrPC=NrPC)
  }) 
  
  #CLOSE CLUSTERS
  snow::stopCluster(cl)
  
  #CHANGE COL AND ROWNAMES AS APPROPRIATE.
  colnames(Predictions) = Start:End
  rownames(Predictions) = rownames(Returns)
  
  #RETURN
  return(Predictions) 
}

