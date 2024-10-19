# CALCULATE PREDICTION OF ALL DAYS IN [START, END], USING HISTORICAL DATA
# START: FIRST DAY OF TRADING
# END: LAST DAY OF TRADING
# H: DAYS TO USE TO CONSTRUCT CORRELATION MATRIX
# L: NR OF DAYS TO USE FOR REGRESSION
# D: HOW MANY DAYS TO USE FOR ROLLING MEAN VOLUME
# ALL ELSE AS BEFORE


#' Add together two numbers
#' @param x A number.
#' @param y A number.
#' @returns A numeric vector.
#' @examples
#' add(1, 1)
#' add(10, 1)
crossTemporalRegressionWithVW <- function(returns, volume, start, end, nr_pc, h, l, b_sensitivity, d, divide) {
  weighted_returns <- constructWeightedReturns(returns = returns, volume = volume, h = h, d = d, divide = divide)

  global_vars <- c(
    "calculateSScore",
    "estimateCoefficeients", "decompose", "extractEigenPortfolio",
    "constructEigenPortfolios", "constructRho", "constructWeightedReturns"
  )

  local_vars <- c("returns", "h", "l", "b_sensitivity", "w")


  # OPEN CORES AND TRANSFER
  cl <- snow::makeCluster(parallel::detectCores() - 1)
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())


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

# DOES crossSectionalRegressionVW, BUT AFTER A TRANSFORMATION OF VOLUME THROUGH MAPPING.
# map_list: A LIST OF FUNCTIONS (F1,F2,..., FJ) S.T. VOLUME TRANSFORMED BY VOLUME -> F(VOLUME) BEFORE WEIGHTING.
mappedCrossTemporalRegressionWithVW <- function(returns, volume, start, end, nr_pc, h, l, b_sensitivity, d, divide, maps) {
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
