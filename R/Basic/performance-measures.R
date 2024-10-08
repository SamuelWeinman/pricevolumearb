###CALCULATE PnL ON PARTICULAR DAY
#P: PREDICTION
#ReturnVector: returns ON THE FOLLOWING DAY (IE DAY OF INTEREST)
#Q: (quantile) LOOK AT TOP k% STRONGEST predictions. 
#  E.G Q = 0.25 means only look at top 25% strongest.
#  TO LOOK AT ALL, USE Quantile = 100
#  CAN BE VECTOR, IN WHICH CASE WILL RETURN SEVERAL VALUES
calculatePnL <- function(returns, p, q) {
  
  #CREATE ARRAY FOR STORING THE PNL
  res <- numeric(length(q)) #i:th entry corresponds to i:th value in  Q
  
  #LOOP THROUGH ALL THE VALUES IN Q:
  for (i in 1:length(q)) {
    
    #SELECT QUANTILE
    q_i <- q[i] 
    
    #FIND THRESHOLD SUCH THAT ONLY PROPORTION q IS ABOVE
    threshold <- quantile(abs(p), 1-q_i) 
    
    #FIND ROW NUMBER OF ALL STOCKS THAT HAVE PREDICTION MAGNITUDE HIGHER THAN THRESHOLD
    index <- which(abs(p) >= threshold) 
    
    #CALCULATE PnL 
    #LOOK AT SIGN OF PREDICTION AND NOT THE PREDICTION ITSELF
    res[i] <- as.numeric(sign(p[index]) %*% log(1+returns[index])) #evaluated against next days true return i
  } 
  
  #RETURN
  return(res)
}


###CALCULATE THE ROLLING STANDARD DEVIATION OF THE PREDICTION
#SO THE R:TH COLUMN WILL BE THE STANDARD DEVIATION OF THE R FIRST predictions
#R: WINDOW WIDTH
#FUNCTION WILL REMOVE THE FIRST R-1 COLUMNS
calculateRollingStandardDeviation <- function(predictions, r) {
  
  #CALCULATE ROLLIGN SD
  #NOTE THAT THE TRANSPOSE IS NECESSARY SO THAT THE SD GOES ALONG THE ROWN AND NOT COLUMN
  #WE THEN TEKE TRANSPOSE AGAIN TO RESTORE FORMAT
  sd <- t(roll_sd(t(as.matrix(predictions)), width <- r))
  
  #REMOVE THE FIRST R-1 VALUES AS THESE ARE JUST NA
  sd <- sd[, -(1:(r-1))] 
  
  #PUT IN DATA FRAME AND ADD NAMES
  sd <- data.frame(sd <- sd)
  rownames(sd) <- rownames(predictions)
  colnames(sd) <- colnames(predictions)[r:ncol(predictions)]
  
  #RETURN
  return(sd)
}

###PERFORM ANALYSIS OF predictions
#THAT IS, CALCULATE PNL, SHARPE, AND PPT.
calculatePerformanceMeasures <- function(returns, predictions, q) {
  
  #GET START AND END COLUMNS
  start <- as.numeric(colnames(predictions)[1])
  End <- as.numeric(colnames(predictions)[ncol(predictions)])
  
  #CALCULATE PNL
  #THE ROWS OF PNL CORRESPOND TO QUANTILE
  #COLUMNS CORRESPOND TO DATES
  pnl <- sapply(1:ncol(predictions), function(i) {
    calculatePnL(returns[, start+i-1], #True Return
                 predictions[, i], #Predicted Return
                 q) #Quantiles
  })
  colnames(pnl) <- start:end
  rownames(pnl) <- q

  #CALCULATE SHARPE RATIO
  #ASSUME SEVERAL VALUES OF Q HAVE BEEN SPECIFIED, I.E. PnL IS A MATRIX
  #THEN WE CAN USE APPLY
  sharpe_ratio <- apply(pnl, 1, mean) / apply(pnl, 1, sd) * sqrt(252)
  
  #CALCULATE PPT
  #APPLY() WILL GIVE THE SUM OF THE PNL FOR EACH OF THE Q
  #NCOL(PREDITCTIONS) WILL GIVE T
  #"ceiling(q * nrow(predictions))" WILL GIVE NR OF STOCKS USED TO CALCULATE PNL, DEPENDING ON QUANTILE q
  ppt <- apply(pnl, 1, sum) / ( ncol(predictions) * ceiling(Q * nrow(predictions)) )
  
  #CALCULATE CUMSUM OF PNL
  #THE COLUMNS OF PNL.CUMSUM ARE THE CUMSUMS
  #COLUMNS VARY BY Q
  pnl_cum_sum <- apply(pnl,1, cumsum)
  
  #RETURN
  return(list(
    pnl = pnl,
    sharpe_ratio = sharpe_ratio,
    ppt = ppt,
    pnl_cum_sum = pnl_cum_sum
  ))
}

###PERFORM FULL ANALYSIS OF PERFORMANCE MEASURES
#FIRST ON NON-STANDARDISED DATA (REGULAR)
#THEN ON STANDARDISED DATA (STANDARD)
#subtract_spy: IF TRUE, COMPARE TO THE returns MINUS THE SPY INDEX, I.E. EXCESS returns ON EACH DAY
performFullAnalysis <- function(returns, predictions, q, r, subtract_spy = FALSE) {
  
  #SUBTRACT SPY IF NECESSARY
  if (subtract_spy) {
    returns_minus_spy  <- sapply(1:ncol(returns), function(i) {
      returns[, i] - returns[1, i] 
    })
    
    colnames(returns_minus_spy) <- colnames(returns)
    rownames(returns_minus_spy) <- rownames(returns)
    returns <- returns_minus_spy
  }

  #ANALYSIS ON NON-STANDARDISED DATA
  #NOTE THAT WE TAKE AWAY THE FIRST R-1 DAYS TO ENSURE SAME FORMAT AS WHEN STANDARDISED
  regular <- calculatePerformanceMeasures(returns, predictions[, -(1:(r-1))], q)
  
  #STANDARDISED 
  #WE REMOVE THE FIRST R-1 COLUMNS AS THERE IS NOT ENOUGH DATA TO STANDARDISE
  pred_standard <- calculateRollingStandardDeviation[, -(1:(r-1))] / RollingSD(predictions, r)
  
  #PERFORMANCE AFTER STANDARDISATION
  standard <- calculatePerformanceMeasures(returns, pred_standard, Q) 
  
  #RETURN
  return(list(
    regular = regular,
    standard = standard
  ))
}


#AS ABOVE, BUT TAKES A LIST OF predictions
#FOR EXAMPLE, THIS CAN BE L DIFFERENT N x T MATRICES WHERE EACH MATRIX CORRESPONDS TO A PARTICULAR METHOD.
performFullAnalysisFromList <- function(returns, predictions_list, q, r, subtract_spy = FALSE) {
  
  #NR OF METHODS
  l <- length(predictions.List)
  
  #CREATE LIST FOR STORING RESULTS
  scores_list <- list()
  
  #LOOP THROUGH ALL predictions, AND FOR EACH PREDICTION GIVE A SEPARATE SCORE.
  for (l_i in 1:l) {
    predictions <- predictions.List[[l_i]] #extract prediction
    score <- performFullAnalysis(returns = returns, 
                     predictions = predictions, 
                     Q = Q, r = r,
                     subtract_spy = subtract_spy) #calculate corresponding results
    scores_list[[l_i]] <- score #add to list
  }
  
  #returns
  return(scores_list)
}




