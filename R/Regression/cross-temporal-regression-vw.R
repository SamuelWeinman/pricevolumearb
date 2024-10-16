# CALCULATE PREDICTION OF ALL DAYS IN [START, END], USING HISTORICAL DATA
# START: FIRST DAY OF TRADING
# END: LAST DAY OF TRADING
# H: DAYS TO USE TO CONSTRUCT CORRELATION MATRIX
# L: NR OF DAYS TO USE FOR REGRESSION
# D: HOW MANY DAYS TO USE FOR ROLLING MEAN VOLUME
# ALL ELSE AS BEFORE
crossTemporalRegressionWithVW <- function(returns, volume, start, end, nr_pc, h, l, b_sensitivity, d, divide) {
  weighted_returns <- constructWeightedReturn(returns = returns, volume = volume, h = h, d = d, divide = divide)

  globalvarlist <- c(
    "calculateSScore",
    "estimateCoefficeients", "decompose", "extractEigenPortfolio",
    "constructEigenPortfolios", "constructRho", "constructWeightedReturn"
  )

  localvarlist <- c("returns", "H", "L", "b_sensitivity", "w")


  # OPEN CORES AND TRANSFER
  cl <- snow::makeCluster(detectCores() - 1)
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, globalvarlist)
  snow::clusterExport(cl, localvarlist, envir = environment())


  # FOR EACH DAY, FIRST STANDARDISE THE RETURNS AND THEN CALUCLATE THE S-SCORE VECTOR (OVER ALL STOCKS)
  s_scores <- snow::parSapply(cl, start:end, function(t) {
    scores <- calculateSScore(
      returns = w[, 1:(t_ - 1)],
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

# DOES CrossSectionRegression.VW, BUT AFTER A TRANSFORMATION OF VOLUME THROUGH MAPPING.
# MAP.list: A LIST OF FUNCTIONS (F1,F2,..., FJ) S.T. VOLUME TRANSFORMED BY VOLUME -> F(VOLUME) BEFORE WEIGHTING.
mappedCrossTemporalRegressionWithVW <- function(returns, volume, start, end, nr_pc, H, L, b_sensitivity, d, divide, maps) {
  k <- length(maps)
  predictions <- list()

  for (k_i in 1:k) {
    map <- maps[[k]]
    mapped_volume <- map(volume)
    predictions[[k]] <- crossTemporalRegressionWithVW(
      returns = returns,
      volume = mapped_volume,
      start = start, end = end,
      nr_pc = nr_pc, h = h, l = l,
      b_sensitivity = b_sensitivity,
      d = d, divide = divide
    )
  }
  return(predictions)
}
