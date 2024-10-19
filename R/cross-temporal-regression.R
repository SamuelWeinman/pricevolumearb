### PERFORMS CT REGRESSION ON ALL DAYS IN INTERVAL [START, END]
# START: FIRST DAY OF TRADING
# END: LAST DAY OF TRADING
# ALL ELSE AS BEFORE
crossTemporalRegression <- function(returns, start, end, nr_pc, h, l, b_sensitivity) {
  # PREPARE CORES#

  # VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  global_vars <- c(
    "calculateSScore",
    "estimateCoefficeients", "decompose", "extractEigenPortfolio",
    "constructEigenPortfolios", "constructRho"
  )

  # VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  local_vars <- c("returns", "h", "l", "b_sensitivity", "nr_pc")


  # OPEN CORES AND TRANSFER
  cl <- snow::makeCluster(parallel::detectCores() - 1)
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())



  # FOR EACH DAY, CALUCLATE THE S-SCORE VECTOR (OVER ALL STOCKS)
  s_scores <- snow::parSapply(cl, start:end, function(t) {
    scores <- calculateSScore(
      returns = returns[, 1:(t - 1)], # using only historical data
      nr_pc = nr_pc,
      h = h, l = l,
      b_sensitivity = b_sensitivity
    )

    # RETURN THE S-SCORE
    return(scores$s)
  })

  # STOP CLUSTERS
  snow::stopCluster(cl)

  # GET FORECASTS (BASED ON HISTORICAL DATA)
  # THE ROW NAMES WILL THE STOCK TICKERS
  # THE COLUMN NAMES WILL BE THE CORRESPONDING COLUMN NUMBERS IN RETURN
  p <- -s_scores
  rownames(p) <- rownames(returns)
  colnames(p) <- start:end

  # RETURN
  return(p)
}
