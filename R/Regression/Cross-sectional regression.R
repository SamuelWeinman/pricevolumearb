library(snow)
library(parallel)




# PERFORM CROSS SECTIONAL REGRESSION ON A PARTICULAR DAY
#  t: column number we are going to trade at
#  H: NR OF DAYS TO USE TO CONSTRUCT CORRELATION MATRIX
#  nr_pc: NUMBER OF PC'S TO USE FOR THE REGRESSION
DayCrossRegression <- function(Returns, t, H, nr_pc) {
  # EXTRACT EIGENPORTFOLIO USING STANDARD PCS
  # USE H DAYS OF HISTORY UP TO TIME t
  E <- extractEigenPortfolio(
    Returns = Returns[, (t - H):(t - 1)],
    nr_pc = nr_pc
  )

  # PERFORM REGRESSION AND EXTRACT PREDICTION
  y <- Returns[, t - 1] # returns on last day
  X <- E$EigenPortfolio # the eigenportfolios
  model <- lm(y ~ X) # regression
  P <- -model$residuals # prediction

  # RETURN THE PREDICTION
  return(p)
}



# CALCULATE PREDICTION OF ALL DAYS IN [START, END], USING HISTORICAL DATA
# START: FIRST DAY OF TRADING
# END: LAST DAY OF TRADING
CrossSectionRegression <- function(Returns, Start, End, H, nr_pc) {
  # PREPARE CORES#

  # VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  global_var_list <- c(
    "DayCrossRegression",
    "extractEigenPortfolio", "constructEigenPortfolios",
    "constructRho"
  )

  # VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  local_var_list <- c("Returns", "H", "nr_pc")

  # OPEN CORES AND TRANSFER
  cl <- snow::makeCluster(detectCores() - 1)
  snow::clusterExport(cl, Globalvarlist)
  snow::clusterExport(cl, Localvarlist, envir = environment())

  # GET PREDICTION OVER THE WHOLE TIME PERIOD
  # ROWS CORRESPOND TO STOCKS
  # THE COLUMNS CORRESPOND TO DAYS IN [START:END]
  predictions <- snow::parSapply(cl, Start:End, function(t) {
    DayCrossRegression(
      Returns = Returns,
      t = t, H = H,
      nr_pc = nr_pc
    )
  })

  # CLOSE CLUSTERS
  snow::stopCluster(cl)

  # CHANGE COL AND ROWNAMES AS APPROPRIATE.
  colnames(Predictions) <- Start:End
  rownames(Predictions) <- rownames(Returns)

  # RETURN
  return(Predictions)
}
