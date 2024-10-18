library(CCA)
library(kernlab)

### PERFORM KCCA ON THE SPACE OF VOLUMES & RETURNS,
### THAT IS, CCA THORGH A KERNEL (IMPLICIT MAPPING)
### THEN PERFORMS CROSS-SECTIONAL REGRESSION USING THE EMBEDDINGS OF RETURNS AND VOLUMES
# STANDARDVOLUME: STANDARDISED VOLUME MATRIX, I.E. AS FRACTION OF MEAN OVER THE LAST (D) DAYS.
# H: NR DAYS OF RETURN HISTORY TO USE
# HV: NR DAYS OF VOLUME HISTORY TO USE
# NRC.R: HOW MANY COMPONENTS TO EXTRACT FROM THE EMBEDDING OF RETURNS.
singleCrossSectionalRegressionWithKCCA <- function(returns, volume, t, h, hv, nr_c_r, nr_c_v) {
  x <- as.matrix(returns[, (t - h):(t - 1)])
  y <- as.matrix(volume[, (t - hv):(t - 1)])

  c <- kcca(x, y, ncomps = max(c(nr_c_r, nr_c_v)))

  x_coeff <- c@xcoef[, 1:nr_c_r]
  y_coeff <- c@ycoef[, 1:nr_c_v]

  x_coeff <- x_coeff / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)
  y_coeff <- y_coeff / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)

  x <- x / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)
  y <- y / apply(as.matrix(returns[, (t - h):(t - 1)]), 1, sd)

  model <- lm(returns[, (t - 1)] ~ x + y)

  prediction <- -1 * model$residuals

  return(prediction)
}


# PERFORMS KCCA CS ON INTERVAL [START, END]
# D: NR OF DAYS TO STANDARDISE OVER
crossSectionalRegressionWithKCCA <- function(returns, volume, t, h, d, hv, nr_c_r, nr_c_v) {
  standardised_volume <- volume / t(roll_mean(t(as.matrix(volume)), width = d))

  global_var_list <- c("singleCrossSectionalRegressionWithKCCA ", "extractEigenPortfolio", "constructEigenPortfolios", "constructRho")
  local_var_list <- c("returns", "h", "hv", "nr_c_r", "nr_c_v", "standardised_volume")


  cl <- snow::makeCluster(detectCores() - 1)
  clusterCall(cl, function() library("kernlab"))
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir = environment())

  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegressionWithKCCA(
      returns = returns,
      volume = standardised_volume,
      t = t,
      h = h, hv = hv,
      nr_c_r = nr_c_r, nr_c_v = nr_c_v
    )
  })

  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}
