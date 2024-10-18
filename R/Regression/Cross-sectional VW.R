### THIS FUNCTION TRANSFORMS ALL RETURNS BY EITHER MULTIPLYING OR DIVIDING BY THE STANDARDISED VOLUME
# DIVIDE: IF TRUE, MULTIPLY BY MEAN(VOLUME)/VOLUME, ELSE MULTIPLY
# D: HOW MANY DAYS TO USE FOR STANDARDISING VOLUME
constructWeightedReturns <- function(returns, volume, d, divide) {

  rolling_mean_volume <- t(roll_mean(t(as.matrix(volume)), width = d))

  if (divide) {
    return(returns * rolling_mean_volume / volume)
  } 

  return(returns * volume / rolling_mean_volume)
}

# PERFORMS CS VOLUME WEIGHTED IN INTERVAL [START, END], USING HISTORICAL DATA
# START: FIRST DAY OF TRADING
# END: LAST DAY OF TRADING
crossSectionalRegressionVW <- function(returns, volume, start, end, h, nr_pc, d, divide) {

  weighted_returns <- constructWeightedReturns(
    returns = returns, volume = volume, d = d,
    divide = divide
  )

  global_var_list <- c(
    "singleCrossSectionalRegression", "constructWeightedReturns",
    "extractEigenPortfolio", "constructEigenPortfolios",
    "constructRho"
  )

  local_var_list <- c("weighted_returns", "h", "nr_pc")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  parallel::clusterCall(cl, function() library("roll"))
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir = environment())


  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegression(
      returns = weighted_returns,
      t = t, h = h,
      nr_pc = nr_pc
    )
  })

  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(predictions)
}



# DOES crossSectionalRegressionVW, BUT AFTER A TRANSFORMATION OF VOLUME THROUGH MAPPING.
# MAP.list: A LIST OF FUNCTIONS (F1,F2,..., FJ) S.T. VOLUME TRANSFORMED BY VOLUME -> F(VOLUME) BEFORE WEIGHTING.
Outside_crossSectionalRegressionVW <- function(returns, volume, start, end, h, nr_pc, d, divide, MAP.list) {
  # NR OF MAPS
  K <- length(MAP.list)

  # CREATE LIST TO STORE PREDICTIONS
  PredictionsList <- list()

  # FOR EACH MAP, TRANSFORM VOLUME TO MAPPEDVOLUME AND THEN PROCEED WITH CS VW REGRESSION.
  for (k in 1:K) {
    map <- MAP.list[[k]] # extract map
    transformed_volume <- map(volume) # map volume
    preds <- crossSectionalRegressionVW(
      returns = returns, # perform calculations with mapped volume
      volume = transformed_volume,
      start = start, end = end,
      h = h,
      nr_pc = nr_pc,
      d = d,
      divide = divide
    )
    PredictionsList[[k]] <- preds # add to list
  }

  # RETURN
  return(PredictionsList)
}
