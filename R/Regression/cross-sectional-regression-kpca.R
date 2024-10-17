library(kernlab)
library(parallel)



# PERFORMS STANDARD CROSS SECTIONAL REGRESSION ON ONE DAY, ALTOUGH THE PCA IS THROUGH A KERNEL
# KERNEL: THE NAME OF THE KERNEL (MUST BE COMPATIBLE WITH KPCA@KERNLAB)
# KPAR: PARAMETERS FOR KERNEL (MUST BE IN A LIST, MUST BE COMPATIBLE WITH KPCA@KERNLAB)
singleCrossSectionRegressionKPCA <- function(returns, t, h, nr_pc, kernel, kpar) {

  returns <- returns[, (t - h):(t - 1)]
  returns <- apply(returns, 2, scale)

  s <- kpca(returns,
    features = nr_pc,
    kernel = kernel, kpar = kpar
  )

  x <- x@rotated / apply(returns, 1, sd)
  y <- returns[, ncol(returns)] # last days of returns
  model <- lm(y ~ X)
  pred <- -model$residuals

  return(pred)
}


# PERFORM KPCA CS REGRESSION OVER AN INTERVAL [START, END]
crossSectionRegressionKPCA <- function(returns, start, end, h, nr_pc, kernel, kpar) {

  global_var_list <- c("singleCrossSectionRegressionKPCA", "constructRho")
  local_var_list <- c("returns", "h", "nr_pc", "kernel", "kpar")

  cl <- snow::makeCluster(detectCores() - 1)
  clusterCall(cl, function() library("kernlab"))
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir = environment())


  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionRegressionKPCA(
      returns = returns,
      t = t,
      h = h, nr_pc = nr_pc,
      kernel = kernel, kpar = kpar
    )
  })

  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}
