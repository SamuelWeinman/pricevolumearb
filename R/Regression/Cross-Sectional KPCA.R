library(kernlab)
library(parallel)



#PERFORMS STANDARD CROSS SECTIONAL REGRESSION ON ONE DAY, ALTOUGH THE PCA IS THROUGH A KERNEL
#KERNEL: THE NAME OF THE KERNEL (MUST BE COMPATIBLE WITH KPCA@KERNLAB)
#KPAR: PARAMETERS FOR KERNEL (MUST BE IN A LIST, MUST BE COMPATIBLE WITH KPCA@KERNLAB)
CrossSectional.KPCA.day = function(Returns, t, H, nr_pc, kernel, kpar) {

  #EXTRACT LAST H DAYS OF HISTORY
  Returns = Returns[, (t-H):(t-1)]
  
  #STANDARDISE COLUMNS PRIOR TO USING KPCA
  Returns = apply(Returns,2, scale)

  #PERFORM KPCA
  s = kpca(Returns, features = nr_pc, 
         kernel = kernel, kpar = kpar)
  
  #EXTRACT PROJECTED DATA
  X = S@rotated
  
  #STANDARDISE ROWS
  X = X/apply(Returns,1, sd)
  
  #PERFORM CROSS-SECTIONAL REJECTION
  y = Returns[, ncol(Returns)] #last days of returns
  model = lm(y~X)
  
  #EXTRACT RESIDUALS AND FORM PREDICTIONS
  Pred = -model$residuals
  
  #RETURN
  return(Pred)
}


#PERFORM KPCA CS REGRESSION OVER AN INTERVAL [START, END]
CrossSectionRegression.KPCA = function(Returns, Start, End, H, nr_pc, kernel, kpar) {
  
  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  globalvarlist = c("CrossSectional.KPCA.day", "constructRho")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  localvarlist = c("Returns","H", "nr_pc", "kernel", "kpar")
  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  clusterCall(cl, function() library("kernlab"))
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment()) 
  
  
  
  #GET PREDICTION OVER THE WHOLE TIME PERIOD
  #ROWS CORRESPOND TO STOCKS
  #THE COLUMNS CORRESPOND TO DAYS IN [START:END]
  predictions = snow::parSapply(cl, Start:End, function(t) {
    CrossSectional.KPCA.day (Returns = Returns,
                             t = t,
                             H=H, nr_pc = nr_pc,
                             kernel = kernel, kpar = kpar)
  }) 
  #CLOSE CLUSTERS
  snow::stopCluster(cl)
  
  #CHANGE COL AND ROWNAMES AS APPROPRIATE.
  colnames(Predictions) = Start:End
  rownames(Predictions) = rownames(Returns)
  
  return(Predictions) 
}






