library(parallel)
library(CCA)

### PERFORM CCA ON THE SPACE OF VOLUMES & RETURNS
### THEN PERFORMS CROSS-TEMPORAL REGRESSION USING THE EMBEDDINGS OF RETURNS AND VOLUMES
# VOLUME SHOULD ALREADy BE STANDARDISED!!
# PARAMETERS AS FOLLOWS:
# T: WHAT DAy WE ARE FORECASTING
# H: NR OF DAyS OF RETURN TO USE FOR EMBEDDING
# HV: NR OF DAyS OF VOLUME TO USE FOR EMBEDDING
# L: NR OF DAyS TO USE FOR REGRESSION ON EIGENRETURNS
# NRC.R: HOW MANy FEAURES OF RETURN EMBEDDINGS TO USE
# NRC.V: HOW MANy FEAURES OF VOLUME EMBEDDINGS TO USE
# b_sensitivity: HOW CLOSE REGRESSION HAS TO BE TO ONE IN ORDER TO REJECT MEAN-REVERSION
singleCrossTemporalRegressionCCA <- function(returns, standardised_volume, t, h, hv, L, nr_c_r, nr_c_v, b_sensitivity) {
  x <- returns[, (t - h):(t - 1)]
  y <- standardised_volume[, (t - hv):(t - 1)]

  c <- cc(x, y)

  x <- as.matrix(x) %*% as.matrix(c$xcoef)
  y <- as.matrix(y) %*% as.matrix(c$ycoef)

  x <- x[, 1:nr_c_r]
  y <- y[, 1:nr_c_r]

  x <- x / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)
  y <- y / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)

  portfolios <- cbind(x, y)
  portfolio_returns <- t(returns[(t - L):(t - 1)]) %*% portfolios

  n <- nrow(returns)
  models <- lapply(1:n, function(i) {
    z <- unlist(returns[i, (t - L):(t - 1)])
    model <- lm(as.numeric(z) ~ portfolio_returns)
    return(model)
  })

  coefficients <- estimateCoefficeients(models, b_sensitivity = b_sensitivity)

  s <- numeric(nrow(returns))
  index <- coefficients$is_mean_reverting == 1
  s[index] <- -coefficients$m[index] / sqrt(coefficients$sigma_eq_squared[index])

  return(list(
    s = s,
    is_mean_reverting = coefficients$is_mean_reverting
  ))
}


crossTemporalRegressionCCA <- function(returns, Volume, start, end, H, HV, L, nr_c_r, nr_c_v, d, b_sensitivity) {
  standardised_volume <- Volume / t(roll_mean(t(as.matrix(Volume)), width = d))

  global_var_list <- c("singleCrossTemporalRegressionCCA", "estimateCoefficeients")
  local_var_list <- c("returns", "H", "HV", "L", "nr_c_r", "nr_c_v", "b_sensitivity", "standardised_volume")

  cl <- snow::makeCluster(detectCores() - 1)
  clusterCall(cl, function() library("CCA"))
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir <- environment())

  predictions <- snow::parSapply(cl, start:end, function(t) {
    s <- singleCrossTemporalRegressionCCA(
      returns = returns, standardised_volume = standardised_volume,
      t = t, H = H, HV = HV,
      L = L, nr_c_r = nr_c_r, nr_c_v = nr_c_v,
      b_sensitivity = b_sensitivity
    )
    p <- -s_scores
    return(p)
  })
  snow::stopCluster(cl)

  rownames(predictions) <- rownames(returns)
  colnames(predictions) <- start:end

  return(predictions)
}
