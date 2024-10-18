# FOR EACH DAY IN THE HISTORY (H), DECOMPOSES THE VOLUMES IN "EIGENVOLUME" PORTFOLIOS, YIELDING RESIDUALS ("OVERTRADED")
# RESIDUALS ARE THEN MAPPED BY RESIDUAL -> EXP(ALPHA * RESIDUALS), AND RETURNS ARE THEN DIVIDED BY THIS AMOUNT.
# THEN PROCEEDS AS NORMAL WITH CS REGRESSION.

# nr_pc_v: HOW MANY PC TO USE WHEN CONSTRUCTED "EIGENVOLUME-PORTFOLIOS",
# I.E. WHENS STUDYING IF (STOCK, DAY) IS OVERTRADED.
# ALPHA: SCALING OF VOLUME RESIDUALS BEFORE TAKING EXPONENTIAL

singleCrossSectionalRegressionOvertraded <- function(volume, returns, t, h, nr_pc_v, nr_pc, alpha) {

  standardised_volume <- volume[, (t - h):(t - 1)] / apply(volume[, (t - h):(t - 1)], 1, sum)
  e_volume <- extractEigenPortfolio(standardised_volume, nr_pc_v)

  overtrade_amounts <- sapply(1:h, function(i) {
    model <- lm(standardised_volume[, i] ~ e_volume$portfolio)
    return(model$residuals)
  })

  overtrade_exp_amounts <- exp(alpha * overtrade_amounts)
  weighted_returns <- returns[, (t - h):(t - 1)] / overtrade_exp_amounts

  e <- extractEigenPortfolio(weighted_returns, nr_pc = nr_pc)
  model <- lm(weighted_returns[, H] ~ e$portfolio)
  return(-model$residuals)
}

# PERFORMS CS OVERTRADED ON INTERVAL [START, END]
crossSectionalRegressionOvertraded <- function(start, end, volume, returns, h, nr_pc_v, alpha, nr_pc) {

  global_var_list <- c(
    "singleCrossSectionalRegressionOvertraded", "extractEigenPortfolio",
    "constructEigenPortfolios",
    "constructRho"
  )
  local_var_list <- c("volume", "returns", "h", "nr_pc_v", "alpha", "nr_pc")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir = environment())


  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegressionOvertraded(
      volume = volume, returns = returns,
      t = t, h = h,
      nr_pc_v = nr_pc_v, nr_pc = nr_pc,
      alpha = alpha
    )
  })

  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}
