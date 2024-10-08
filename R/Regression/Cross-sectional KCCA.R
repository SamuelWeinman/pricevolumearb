library(CCA)
library(kernlab)

###PERFORM KCCA ON THE SPACE OF VOLUMES & RETURNS, 
###THAT IS, CCA THORGH A KERNEL (IMPLICIT MAPPING)
###THEN PERFORMS CROSS-SECTIONAL REGRESSION USING THE EMBEDDINGS OF RETURNS AND VOLUMES
#STANDARDVOLUME: STANDARDISED VOLUME MATRIX, I.E. AS FRACTION OF MEAN OVER THE LAST (D) DAYS.
#H: NR DAYS OF RETURN HISTORY TO USE
#HV: NR DAYS OF VOLUME HISTORY TO USE
#NRC.R: HOW MANY COMPONENTS TO EXTRACT FROM THE EMBEDDING OF RETURNS.
DayCrossRegression.KCCA <- function(Returns,Volume, t,H,HV,NrC.R, NrC.V) {
  
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

  #DIVIDE EACH ROW BY THE STANDARD DEVIATION OF THE ROW 
  X = X / apply(as.matrix(Returns[,(t-H):(t-1)]),1,sd)
  Y = Y / apply(as.matrix(Returns[,(t-H):(t-1)]),1,sd)
  
  
  #FIT CROSS-SECTIONAL MODEL
  model = lm(Returns[,(t-1)] ~ X + Y)
  
  #EXTRACT RESIDUALS AND FORM PREDICTION
  Prediction = -model$residuals
  
  #RETURN
  return(Prediction)
}


#PERFORMS KCCA CS ON INTERVAL [START, END]
#D: NR OF DAYS TO STANDARDISE OVER
CrossSectionRegression.KCCA <- function(Returns,Volume, Start, End, H,d,HV,NrC.R, NrC.V) {
  
  #STANDRDISE VOLUME
  #HERE, WE DIVIDE VOLUME BY THE AVERAGE VOLUME DURING THE LAST D DAYS (INCLUDING TODAY)
  StandardisedVolume = Volume / t(roll_mean(t(as.matrix(Volume)), width = d))
  
  #PREPARE CORES#
  
  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  Globalvarlist = c("DayCrossRegression.KCCA",
                    "ExtractEigenPortfolio", "ConstructEigenPortfolios", 
                    "ConstructRho")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  Localvarlist = c("Returns","H","NrC.R","NrC.V","HV","StandardisedVolume")
  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  clusterCall(cl, function() library("kernlab"))
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment()) 
  
  #GET PREDICTION OVER THE WHOLE TIME PERIOD
  #ROWS CORRESPOND TO STOCKS
  #THE COLUMNS CORRESPOND TO DAYS IN [START:END]
  Predictions = snow::parSapply(cl, Start:End, function(t) {
    DayCrossRegression.KCCA(Returns = Returns,
                           Volume = StandardisedVolume, 
                           t = t,
                           H=H,HV = HV,
                           NrC.R = NrC.R, NrC.V = NrC.V)
  }) 
  
  #CLOSE CLUSTERS
  snow::stopCluster(cl)
  
  #CHANGE COL AND ROWNAMES AS APPROPRIATE.
  colnames(Predictions) = Start:End
  rownames(Predictions) = rownames(Returns)
  
  #RETURN
  return(Predictions) 
}





