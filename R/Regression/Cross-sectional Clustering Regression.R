library(parallel)


#CONSTRUCT CLUSTERS OF TIME SERIES FROM CORRELATION MATRIX
#I.E. EACH STOCK IS MAPPED TO ONE OF K CLUSTERS
#RHO: CORRELATION MATRIX
#K: NR OF CLUSTERS
#MinSize: MINUMUM NR OF MEMBERS PER CLUSTER, OR ELSE WILL USE FEWER CLUSTERS
ConstructClusters = function(rho, k, MinSize) {

  #CONSTRUCT DISTANCE MATRIX
  d = as.dist(1-rho)
  
  #BUILD DENDOGRAM
  tree = hclust(d)

  #EXTRACT LABELS
  labels = as.numeric(cutree(tree, k = k))

  #ENSURE ARE CLUSTERS ARE LARGE ENOUGH, OR ELSE USE ONE FEWER CLUSTER
  k.New = k
  
  #CREATE INFINITE LOOP
  while(1 == 1) {
    
    #IF CLUSTER TOO SMALL, UPDATE K.NEW AND CONTINUE
    #IF LARGE ENOUGHT, EXIT LOOP
    if (min(table(labels)) < MinSize) {
      k.New = k.New - 1
      labels = as.numeric(cutree(tree, k = k.New))
    } else {
      break
    }
  }
  
  #GET CLUSTERS
  clusters = lapply(1:k.New, function(i) {
    which(labels == i)
  })
  
  #RETURN
  return(clusters)
}


#PERFORMS CLUSTERING CROSS-SECTIONAL REGRESSION FOR ONE DAY (T)
# ALPHA: WEIGHTING; COR =  ALPHA * COR(RETURNS) + (1-ALPHA) COR(VOLUMES) 
DayCrossRegression.Cluster <- function(Returns, Volume, t,H,nr_pc,k,MinSize, alpha) {
  
  #EXTRACT EIGENPORTFOLIO
  E = ExtractEigenPortfolio(Returns = Returns[, (t-H):(t-1)], 
                            nr_pc = nr_pc) 
  
  #CONSTRUCT WRIGHTED CORRELATION MATRIX
  rho = alpha* ConstructRho(Returns[, (t-H):(t-1)]) + (1-alpha)*ConstructRho(Volume[, (t-H):(t-1)])
  
  
  #EXTRACT CLUSTERS
  Clusters = ConstructClusters(rho = rho, k = k, MinSize = MinSize)
  
  #TRUE NR OF CLUSTERS, WHICH MIGHT BE DIFFERENT FROM ORIGINAL K IF CLUSTERS TOO SMALL
  k = length(Clusters)
  
  #FOR EACH CLUSTER, FIT A SEPARATE LINEAR REGRESSION
  #NEED TO STANDARDISE RESIDUALS TO BE ABLE TO COMPARE ACCROSS MODELS
  Predictions.List = sapply(1:k, function(i){
    index = Clusters[[i]] #EXTRACT I:TH CLUSTER
    y = Returns[index, t-1] #returns on last day
    X= E$EigenPortfolio[index, ] #the eigenportfolios
    model = lm(y~X)
    P = -model$residuals
    P = P / sd(P)
  })
  
  #FINALLY, PUT ALL RESIDUALS INTO A SINGLE ARRAY IN APPROPRIATE ORDER
  #IF ALL IS JUST ONE CLUSTER, WE TRANSFORM LIST LIST INTO ARRAY
  #OTHERWISE, WE HAVE TO BE CAREFUL ABOUT THE ORDERING
  if (k == 1) {
    Predictions = Predictions.List
  } else {
    Predictions= numeric(nrow(Returns))
    for (i in 1:k) {
      Predictions[Clusters[[i]]] = Predictions.List[[i]]
    }
  }
  #RETURN
  return(list(Predictions = Predictions,
              k = k))
}

CrossSectionRegression.Cluster <- function(Returns, Volume, Start, End, H, nr_pc,k,MinSize, alpha) {
  
  #PREPARE CORES#
  
  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  globalvarlist = c("DayCrossRegression.Cluster", "ConstructClusters",
                    "ExtractEigenPortfolio", "ConstructEigenPortfolios", 
                    "ConstructRho")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  localvarlist = c("Returns","Volume","H","nr_pc","k", "MinSize", "alpha")
  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment()) 
  
  #GET PREDICTION OVER THE WHOLE TIME PERIOD
  #ROWS CORRESPOND TO STOCKS
  #THE COLUMNS CORRESPOND TO DAYS IN [START:END]
  predictions = snow::parSapply(cl, Start:End, function(t) {
    DayCrossRegression.Cluster(Returns = Returns,
                               Volume = Volume,
                               t=t, H = H,
                               nr_pc = nr_pc,
                               k = k, MinSize = MinSize,
                               alpha = alpha)
    
  }) 
  
  #CLOSE CLUSTERS
  snow::stopCluster(cl)

  #EXTRACT PREDICTIONS AND NR OF CLUSTERS
  k = unlist(Predictions[2, ]) #NR OF CLUSTERS
  Predictions = matrix(unlist(Predictions[1, ]), ncol = End-Start+1)
  
  #CHANGE COL AND ROWNAMES AS APPROPRIATE.
  colnames(Predictions) = Start:End
  rownames(Predictions) = rownames(Returns)
  
  #RETURN
  return(list(Predictions = Predictions,
              k = k)) 
}




            

