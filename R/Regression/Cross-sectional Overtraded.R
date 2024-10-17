# FOR EACH DAY IN THE HISTORY (H), DECOMPOSES THE VOLUMES IN "EIGENVOLUME" PORTFOLIOS, YIELDING RESIDUALS ("OVERTRADED")
# RESIDUALS ARE THEN MAPPED BY RESIDUAL -> EXP(ALPHA * RESIDUALS), AND RETURNS ARE THEN DIVIDED BY THIS AMOUNT.
# THEN PROCEEDS AS NORMAL WITH CS REGRESSION.

# nr_pc.V: HOW MANY PC TO USE WHEN CONSTRUCTED "EIGENVOLUME-PORTFOLIOS",
# I.E. WHENS STUDYING IF (STOCK, DAY) IS OVERTRADED.
# ALPHA: SCALING OF VOLUME RESIDUALS BEFORE TAKING EXPONENTIAL

Day.CS.Overtrade <- function(Volume, Returns, t, H, nr_pc.V, nr_pc, alpha) {
  # STANDARDISE VOLUME: DISTRIBUTION OF VOLUME ON THE PRVIOUS H DAYS. I.E. NOT A ROLLING WINDOW APPROACH AS BEFORE!
  # FOR ANY T, WILL COMPARE ALL THE HISTORY TO THE H DAYS BEFORE T.
  standardised_volume <- Volume[, (t - H):(t - 1)] / apply(Volume[, (t - H):(t - 1)], 1, sum)

  # CONSTRUCT "EIGENPORTFOLIO" OF VOLUME
  E.V <- extractEigenPortfolio(standardised_volume, nr_pc.V)

  # CALCULATE BY HOW MUCH IT'S STOCK IS OVERTRADED OVERTRADING
  # HERE Overtraded_{I, T} IS THE AMOUNT THAT STOCK I WAS OVERTRADED ON DAY T
  Overtraded <- sapply(1:H, function(i) {
    y <- standardised_volume[, i] # standardise volume on the day
    model <- lm(y ~ E.V$EigenPortfolio) # regress on eigenportfolios
    return(model$residuals) # extract residuals
  })

  # EXPONENTIAL RESIDUALS
  Overtraded <- exp(alpha * Overtraded)

  # CONSTRUCT WEIGHTED RETURN, PENALISING LARGE TRADING DAYS
  WeightedReturn <- Returns[, (t - H):(t - 1)] / Overtraded

  # NOW, PROCEED AS BEFORE (CS)#

  # CONSTRUCT EIGENPORTFOLIOS
  E <- extractEigenPortfolio(WeightedReturn, nr_pc = nr_pc)

  # REGRESS WEIGHTED RETURNS ON EIGENPORTFOLIOS
  y <- WeightedReturn[, H]
  model <- lm(y ~ E$EigenPortfolio)
  Prediction <- -model$residuals

  # RETURN
  return(Prediction)
}

# PERFORMS CS OVERTRADED ON INTERVAL [START, END]
CrossSectionRegression.OverTrade <- function(start, end, Volume, Returns, H, nr_pc.V, alpha, nr_pc) {
  # PREPARE CORES#

  # VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  global_var_list <- c(
    "Day.CS.Overtrade", "extractEigenPortfolio",
    "constructEigenPortfolios",
    "constructRho"
  )

  # VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  local_var_list <- c("Volume", "Returns", "H", "nr_pc.V", "alpha", "nr_pc")


  # OPEN CORES AND TRANSFER
  cl <- snow::makeCluster(detectCores() - 1)
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir = environment())


  # GET PREDICTION OVER THE WHOLE TIME PERIOD
  # ROWS CORRESPOND TO STOCKS
  # THE COLUMNS CORRESPOND TO DAYS IN [START:END]
  predictions <- snow::parSapply(cl, start:end, function(t) {
    Day.CS.Overtrade(
      Volume = Volume, Returns = Returns,
      t = t, H = H,
      nr_pc.V = nr_pc.V, nr_pc = nr_pc,
      alpha = alpha
    )
  })

  # CLOSE CLUSTERS
  snow::stopCluster(cl)

  # CHANGE COL AND ROWNAMES AS APPROPRIATE.
  colnames(Predictions) <- start:end
  rownames(Predictions) <- rownames(Returns)

  # RETURNS
  return(Predictions)
}
