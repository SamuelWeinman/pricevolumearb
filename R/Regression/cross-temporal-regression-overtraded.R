# FOR EACH DAY IN THE HISTORY (H), DECOMPOSES THE VOLUMES IN "EIGENVOLUME" PORTFOLIOS, YIELDING RESIDUALS ("OVERTRADED")
# RESIDUALS ARE THEN MAPPED BY RESIDUAL -> EXP(ALPHA * RESIDUALS), AND RETURNS ARE THEN DIVIDED BY THIS AMOUNT.
# THEN PROCEEDS AS NORMAL WITH CROSS TEMPORA; REGRESSION.

# nr_pc.V: HOW MANY PC TO USE WHEN CONSTRUCTED "EIGENVOLUME-PORTFOLIOS",
# I.E. WHENS STUDYING IF (STOCK, DAY) IS OVERTRADED.
# ALPHA: SCALING OF VOLUME RESIDUALS BEFORE TAKING EXPONENTIAL

# NOTE: VOLUMES ARE DECOMPOSED USING CS REGRESSION, ONLY RETURNS ARE DECOMPOSED WITH CROSS-TEMPORAL REGRESSION
singleCrossTemporalRegressionOvertrade <- function(volume, returns, t, h, nr_pc.V, nr_pc, alpha, l, b_sensitivity) {

  standardised_volume <- volume[, (t - h):(t - 1)] / apply(volume[, (t - h):(t - 1)], 1, sum)

  eigen <- extractEigenPortfolio(standardised_volume, nr_pc.V)


  overtraded_log <- sapply(1:h, function(i) {
    y <- standardised_volume[, i] # standardise volume on the day
    model <- lm(y ~ eigen$portfolio) # regress on eigenportfolios
    return(model$residuals) # extract residuals
  })

  overtraded <- exp(alpha * overtraded_log)
  weighted_returns <- returns[, (t - h):(t - 1)] / overtraded

  models <- decompose(
    returns = weighted_returns,
    h = h,
    l = l,
    nr_pc = nr_pc
  )
  coefficients <- estimateCoefficeients(models, b_sensitivity)



  s <- numeric(nrow(returns))
  mean_reverting_indices <- coefficients$is_mean_reverting == 1 
  s[mean_reverting_indices] <- -coefficients$m[index] / sqrt(coefficients$sigma_eq_squared[mean_reverting_indices])

  # RETURN
  return(list(
    s = s,
    is_mean_reverting = coefficients$is_mean_reverting
  ))
}


### PERFORMS CT OVERTRADED ON INTERVAL [START, END]
# START: FIRST DAY OF TRADING
# END: LAST DAY OF TRADING
# ALL ELSE AS BEFORE
crossTemporalRegressionOvertrade <- function(volume, returns, start, end, h, nr_pc.V, nr_pc, alpha, l, b_sensitivity) {
  # PREPARE CORES#

  # VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  global_var_list <- c(
    "singleCrossTemporalRegressionOvertrade",
    "estimateCoefficeients", "decompose", "extractEigenPortfolio",
    "constructEigenPortfolios", "constructRho"
  )

  # VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  local_var_list <- c(
    "returns", "volume",
    "h", "l", "b_sensitivity",
    "nr_pc.V", "nr_pc", "alpha", "b_sensitivity"
  )


  # OPEN CORES AND TRANSFER
  cl <- snow::makeCluster(parallel::detectCores() - 1)
  parallel::clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir = environment())



  # FOR EACH DAY, CALUCLATE THE S-SCORE VECTOR (OVER ALL STOCKS)
  predictions <- snow::parSapply(cl, start:end, function(t) {
    s <- singleCrossTemporalRegressionOvertrade(
      volume = volume, returns = returns, t = t,
      h = h, nr_pc.V = nr_pc.V, nr_pc = nr_pc,
      alpha = alpha, l = l, b_sensitivity = b_sensitivity
    ) 

    return(-s$score)
  })

  snow::stopCluster(cl)
  rownames(predictions) <- rownames(returns)
  colnames(predictions) <- start:end

  # RETURN
  return(predictions)
}
