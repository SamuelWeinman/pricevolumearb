# PERFORM CROSS SECTIONAL REGRESSION ON A PARTICULAR DAY
#  t: column number we are going to trade at
#  H: NR OF DAYS TO USE TO CONSTRUCT CORRELATION MATRIX
#  nr_pc: NUMBER OF PC'S TO USE FOR THE REGRESSION
singleCrossSectionalRegression <- function(returns, t, h, nr_pc) {
  e <- extractEigenPortfolio(
    returns = returns[, (t - h):(t - 1)],
    nr_pc = nr_pc
  )

  model <- lm(returns[, t - 1] ~ e$portfolio)
  p <- -model$residuals
  return(p)
}



# CALCULATE PREDICTION OF ALL DAYS IN [START, END], USING HISTORICAL DATA
# START: FIRST DAY OF TRADING
# END: LAST DAY OF TRADING
crossSectionalRegression <- function(returns, start, end, h, nr_pc) {
  global_var_list <- c(
    "singleCrossSectionalRegression",
    "extractEigenPortfolio", "constructEigenPortfolios",
    "constructRho"
  )

  local_var_list <- c("returns", "h", "nr_pc")

  cl <- snow::makeCluster(detectCores() - 1)
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir = environment())

  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegression(
      returns = returns,
      t = t, h = h,
      nr_pc = nr_pc
    )
  })
  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}
