library(roll)

###CALCULATE PnL ON PARTICULAR DAY
#P: PREDICTION
#ReturnVector: RETURNS ON THE FOLLOWING DAY (IE DAY OF INTEREST)
#Q: (quantile) LOOK AT TOP k% STRONGEST PREDICTIONS. 
#  E.G Q=0.25 means only look at top 25% strongest.
#  TO LOOK AT ALL, USE Quantile=100
#  CAN BE VECTOR, IN WHICH CASE WILL RETURN SEVERAL VALUES
CalculatePnL <- function(ReturnsVector,P, Q) {
  
  #CREATE ARRAY FOR STORING THE PNL
  PnL = numeric(length(Q)) #i:th entry corresponds to i:th value in  Q
  
  #LOOP THROUGH ALL THE VALUES IN Q:
  for (i in 1:length(Q)) {
    
    #SELECT QUANTILE
    q = Q[i] 
    
    #FIND THRESHOLD SUCH THAT ONLY PROPORTION q IS ABOVE
    threshold = quantile(abs(P), 1-q) 
    
    #FIND ROW NUMBER OF ALL STOCKS THAT HAVE PREDICTION MAGNITUDE HIGHER THAN THRESHOLD
    Index = which(abs(P) >= threshold) 
    
    #CALCULATE PnL 
    #LOOK AT SIGN OF PREDICTION AND NOT THE PREDICTION ITSELF
    PnL[i] = as.numeric(sign(P[Index]) %*% log(1+ReturnsVector[Index])) #evaluated against next days true return 
  } 
  
  #RETURN
  return(PnL)
}


###CALCULATE THE ROLLING STANDARD DEVIATION OF THE PREDICTION
#SO THE R:TH COLUMN WILL BE THE STANDARD DEVIATION OF THE R FIRST PREDICTIONS
#R: WINDOW WIDTH
#FUNCTION WILL REMOVE THE FIRST R-1 COLUMNS
RollingSD <- function(Predictions, r) {
  
  #CALCULATE ROLLIGN SD
  #NOTE THAT THE TRANSPOSE IS NECESSARY SO THAT THE SD GOES ALONG THE ROWN AND NOT COLUMN
  #WE THEN TEKE TRANSPOSE AGAIN TO RESTORE FORMAT
  SD = t(roll_sd(t(as.matrix(Predictions)), width = r))
  
  #REMOVE THE FIRST R-1 VALUES AS THESE ARE JUST NA
  SD = SD[,-(1:(r-1))] 
  
  #PUT IN DATA FRAME AND ADD NAMES
  SD = data.frame(SD=SD)
  rownames(SD)=rownames(Predictions)
  colnames(SD)= colnames(Predictions)[r:ncol(Predictions)]
  
  #RETURN
  return(SD)
}

###PERFORM ANALYSIS OF PREDICTIONS
#THAT IS, CALCULATE PNL, SHARPE, AND PPT.
PerformanceMeasures <- function(Returns, Predictions,Q) {
  
  #GET START AND END COLUMNS
  Start = as.numeric(colnames(Predictions)[1])
  End = as.numeric(colnames(Predictions)[ncol(Predictions)])
  
  #CALCULATE PNL
  #THE ROWS OF PNL CORRESPOND TO QUANTILE
  #COLUMNS CORRESPOND TO DATES
  PnL = sapply(1:ncol(Predictions), function(i) {
    CalculatePnL(ReturnsVector=Returns[,Start+i-1], #True Return
                 P=Predictions[,i], #Predicted Return
                 Q=Q) #Quantiles
  })
  colnames(PnL)=Start:End
  rownames(PnL)=Q

  #CALCULATE SHARPE RATIO
  #ASSUME SEVERAL VALUES OF Q HAVE BEEN SPECIFIED, I.E. PnL IS A MATRIX
  #THEN WE CAN USE APPLY
  Sharpe = apply(PnL,1,mean) / apply(PnL,1,sd) * sqrt(252)
  
  #CALCULATE PPT
  #APPLY() WILL GIVE THE SUM OF THE PNL FOR EACH OF THE Q
  #NCOL(PREDITCTIONS) WILL GIVE T
  #"ceiling(Q * nrow(Predictions))" WILL GIVE NR OF STOCKS USED TO CALCULATE PNL, DEPENDING ON QUANTILE q
  PPT = apply(PnL, 1, sum) / ( ncol(Predictions) * ceiling(Q * nrow(Predictions)) )
  
  #CALCULATE CUMSUM OF PNL
  #THE COLUMNS OF PNL.CUMSUM ARE THE CUMSUMS
  #COLUMNS VARY BY Q
  PnL.Cumsum = apply(PnL,1,cumsum)
  
  #RETURN
  return(list(
    PnL=PnL,
    Sharpe=Sharpe,
    PPT = PPT,
    PnL.Cumsum = PnL.Cumsum
  ))
}

###PERFORM FULL ANALYSIS OF PERFORMANCE MEASURES
#FIRST ON NON-STANDARDISED DATA (REGULAR)
#THEN ON STANDARDISED DATA (STANDARD)
#SUBTRACTSPY: IF TRUE, COMPARE TO THE RETURNS MINUS THE SPY INDEX,I.E. EXCESS RETURNS ON EACH DAY
Analysis <- function(Returns, Predictions, Q, r, SubtractSpy = FALSE) {
  
  #SUBTRACT SPY IF NECESSARY
  if (SubtractSpy) {
    Returns.MinusSPY  = sapply(1:ncol(Returns), function(i) {
      Returns[,i] - Returns[1,i] 
    })
    
    colnames(Returns.MinusSPY) = colnames(Returns)
    rownames(Returns.MinusSPY) = rownames(Returns)
    Returns = Returns.MinusSPY
  }

  #ANALYSIS ON NON-STANDARDISED DATA
  #NOTE THAT WE TAKE AWAY THE FIRST R-1 DAYS TO ENSURE SAME FORMAT AS WHEN STANDARDISED
  Regular =PerformanceMeasures(Returns, Predictions[,-(1:(r-1))], Q)
  
  #STANDARDISED 
  #WE REMOVE THE FIRST R-1 COLUMNS AS THERE IS NOT ENOUGH DATA TO STANDARDISE
  PredStandard = Predictions[,-(1:(r-1))] / RollingSD(Predictions, r)
  
  #PERFORMANCE AFTER STANDARDISATION
  Standard = PerformanceMeasures(Returns, PredStandard, Q) 
  
  #RETURN
  return(list(
    Regular = Regular,
    Standard = Standard
  ))
}


#AS ABOVE, BUT TAKES A LIST OF PREDICTIONS
#FOR EXAMPLE, THIS CAN BE L DIFFERENT N x T MATRICES WHERE EACH MATRIX CORRESPONDS TO A PARTICULAR METHOD.
Analysis.List = function(Returns, Predictions.List, Q, r, SubtractSpy=FALSE) {
  
  #NR OF METHODS
  L= length(Predictions.List)
  
  #CREATE LIST FOR STORING RESULTS
  ScoresList = list()
  
  #LOOP THROUGH ALL PREDICTIONS, AND FOR EACH PREDICTION GIVE A SEPARATE SCORE.
  for (l in 1:L) {
    Predictions = Predictions.List[[l]] #extract prediction
    Score = Analysis(Returns=Returns, 
                     Predictions=Predictions, 
                     Q=Q, r=r,
                     SubtractSpy=SubtractSpy) #calculate corresponding results
    ScoresList[[l]] = Score #add to list
  }
  
  #RETURNS
  return(ScoresList)
}




