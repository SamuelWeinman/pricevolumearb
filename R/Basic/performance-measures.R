### CALCULATE PnL ON PARTICULAR DAY
# P: PREDICTION
# ReturnVector: returns ON THE FOLLOWING DAY (IE DAY OF INTEREST)
# Q: (quantile) LOOK AT TOP k% STRONGEST predictions.
#  E.G Q = 0.25 means only look at top 25% strongest.
#  TO LOOK AT ALL, USE Quantile = 100
#  CAN BE VECTOR, IN WHICH CASE WILL RETURN SEVERAL VALUES
calculatePnL <- function(returns, p, q) {
  res <- numeric(length(q))

  for (i in 1:length(q)) {
    q_i <- q[i]
    threshold <- quantile(abs(p), 1 - q_i)
    index <- which(abs(p) >= threshold)
    res[i] <- as.numeric(sign(p[index]) %*% log(1 + returns[index]))
  }
  return(res)
}


### CALCULATE THE ROLLING STANDARD DEVIATION OF THE PREDICTION
# SO THE R:TH COLUMN WILL BE THE STANDARD DEVIATION OF THE R FIRST predictions
# R: WINDOW WIDTH
# FUNCTION WILL REMOVE THE FIRST R-1 COLUMNS
calculateRollingStandardDeviation <- function(predictions, r) {
  sd <- t(roll_sd(t(as.matrix(predictions)), width <- r))[, -(1:(r - 1))][, -(1:(r - 1))]
  sd_df <- data.frame(sd = sd)
  rownames(sd_df) <- rownames(predictions)
  colnames(sd_df) <- colnames(predictions)[r:ncol(predictions)]
  return(sd_df)
}

### PERFORM ANALYSIS OF predictions
# THAT IS, CALCULATE PNL, SHARPE, AND PPT.
calculatePerformanceMeasures <- function(returns, predictions, q) {
  start <- as.numeric(colnames(predictions)[1])
  End <- as.numeric(colnames(predictions)[ncol(predictions)])
  pnl <- sapply(1:ncol(predictions), function(i) {
    return(calculatePnL(
      returns = returns[, start + i - 1],
      p = predictions[, i],
      q = q
    ))
  })
  colnames(pnl) <- start:end
  rownames(pnl) <- q

  sharpe_ratio <- apply(pnl, 1, mean) / apply(pnl, 1, sd) * sqrt(252)
  ppt <- apply(pnl, 1, sum) / (ncol(predictions) * ceiling(Q * nrow(predictions)))
  pnl_cum_sum <- apply(pnl, 1, cumsum)

  return(list(
    pnl = pnl,
    sharpe_ratio = sharpe_ratio,
    ppt = ppt,
    pnl_cum_sum = pnl_cum_sum
  ))
}

### PERFORM FULL ANALYSIS OF PERFORMANCE MEASURES
# FIRST ON NON-STANDARDISED DATA (REGULAR)
# THEN ON STANDARDISED DATA (STANDARD)
# subtract_spy: IF TRUE, COMPARE TO THE returns MINUS THE SPY INDEX, I.E. EXCESS returns ON EACH DAY
performFullAnalysis <- function(returns, predictions, q, r, subtract_spy = FALSE) {
  if (subtract_spy) {
    returns_minus_spy <- sapply(1:ncol(returns), function(i) {
      returns[, i] - returns[1, i]
    })

    colnames(returns_minus_spy) <- colnames(returns)
    rownames(returns_minus_spy) <- rownames(returns)
    returns <- returns_minus_spy
  }

  regular <- calculatePerformanceMeasures(returns, predictions[, -(1:(r - 1))], q)
  pred_standard <- calculateRollingStandardDeviation[, -(1:(r - 1))] / RollingSD(predictions, r)
  standard <- calculatePerformanceMeasures(returns, pred_standard, Q)

  return(list(
    regular = regular,
    standard = standard
  ))
}


# AS ABOVE, BUT TAKES A LIST OF predictions
# FOR EXAMPLE, THIS CAN BE L DIFFERENT N x T MATRICES WHERE EACH MATRIX CORRESPONDS TO A PARTICULAR METHOD.
performFullAnalysisFromList <- function(returns, predictions_list, q, r, subtract_spy = FALSE) {
  l <- length(predictions.List)
  scores_list <- list()
  for (l_i in 1:l) {
    predictions <- predictions.List[[l_i]] # extract prediction
    score <- performFullAnalysis(
      returns = returns,
      predictions = predictions,
      Q = Q, r = r,
      subtract_spy = subtract_spy
    ) # calculate corresponding results
    scores_list[[l_i]] <- score # add to list
  }
  return(scores_list)
}
