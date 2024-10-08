setwd("~/MSc Statistical Science/Dissertation")

###READ DATA
Close = read.csv(file = "Data//CLOSE.csv", header = TRUE)
Close = as.data.frame(Close[,-1], row.names = as.character(Close[,1]))
colnames(Close) = str_remove(colnames(Close), "X") 

Volume = read.csv("Data//VOLUME.csv", header = TRUE)
Volume = as.data.frame(Volume[,-1], row.names = as.character(Volume[,1]))
colnames(Volume) = str_remove(colnames(Volume), "X") #remove the X character



###CONSTRUCT RETURN MATRIX
#INPUTS AS FOLLOWS:
#  CLOSE: MATRIX OF CLOSING PRICES (AS ABOVE)
#  H: NUMBER OF DAYS TO GET RETURN ON, E.G. h = 1 CORRESPONDS TO ONE DAY RETURNS
#LOOKING AT CLOSE-CLOSE PRICES
ConstructReturnMatrix <- function(CLOSE, h) {
  T = ncol(CLOSE)
  Returns =( CLOSE[,(h+1):T]-CLOSE[,1:(T-h)] ) / CLOSE[,1:(T-h)] 
  return(Returns)
}


###REMOVE DAYS WITH EXTREME OUTCOMES
#  Returns: MATRIX OF RETURNS (SEE ABOVE)
#  PropStocksZero: PROPORTION OF STOCKS ALLOWED TO HAVE ZERO GROWTH
DatesToRemove <- function(Returns, PropStocksZero) {
  
  
  ###ALL BELOW IS AS FUNCTION OF DAYS###
  
  #CALCULATE PROPORTION OF STOCKS WITH ZERO GROWTH
  PropWithNoGrowth = apply(Returns, 2, function(column) { mean(column == 0)})
  
  #COLUMNS WITH LITTLE GROWTH (TO BE DELETED)
  ColumnsWithLittleGrowth = which(PropWithNoGrowth > PropStocksZero)
  
  #CORRESPONDING DAYS
  DaysWithLittleGrowth = colnames(Returns)[ColumnsWithLittleGrowth]
  
  #RETURN
  return(list(
    ColumnNumbers = ColumnsWithLittleGrowth,
    Dates = DaysWithLittleGrowth
  ))
}


###REMOVE STOCKS WITH EXTREME OUTCOMES
###WE SHALL REMOVE STOCKS THAT EITHER HAD TOO MUCH GROWTH ON TOO MANY DAYS, OR HAD NO GROWTH ON TOO MANY DAYS
#  Returns: matrix of returns (see above)
#  PropDaysZero: proportion of days stock is allowed to have zero growth, or else stock is removed
#  PropDaysUpperGrowth: proportion of stock allowed to have high growth, or stock day is removed
#  UpperBoundGrowth: threshold for weather growth is considered high or not. For example, 1 corresponds to 100% growth

StocksToRemove <- function(Returns, PropDaysZero, PropDaysUpperGrowth, UpperBoundGrowth) {
  
  ###ALL BELOW IS AS FUNCTION OF STOCKS###
  
  #PROPORTION OF DAYS WITH ZERO GROWTH
  PropWithNoGrowth = apply(Returns, 1, function(row) { mean(row == 0)})
  
  #ROWS WHERE THERE WAS NOT ENOUGH GROWTH
  RowsWithLittleGrowth = which(PropWithNoGrowth > PropDaysZero)
  
  #PROPORTION OF DAYS WITH HIGH GROWTH
  PropHighGrowth = apply(Returns, 1, function(row) { mean(row>UpperBoundGrowth)})
  
  #ROWS WHERE THERE WAS TOO MUCH GROWTH
  RowsWithHighGrowth = which(PropHighGrowth > PropDaysUpperGrowth)
  
  
  #REMOVE ROWS WITH EITHER TOO MUCH OR TOO LITTLE GROWTH
  RowsToRemove = c(RowsWithHighGrowth, RowsWithLittleGrowth)
  
  #CORRESPONDING STOCK NAMES
  StocksToRemove = rownames(Returns)[RowsToRemove]
  
  
  #RETURN
  return(list(
    RowNumbers =RowsToRemove,
    Stocks = StocksToRemove
  ))
}


###PUT TOGETHER INTO A SINGLE FUNCTION
###INPUTS AS ABOVE
SanityCheck <- function(Volume, Returns, PropStocksZero, PropDaysZero, PropDaysUpperGrowth, UpperBoundGrowth) {
  
  #COLUMNS TO REMOVE
  Columns = DatesToRemove(Returns, PropStocksZero)
  
  #ROWS TO REMOVE
  Rows = StocksToRemove(Returns, PropDaysZero, PropDaysUpperGrowth, UpperBoundGrowth) 
  

  #REMOVE ROWS IF NECESSARY I.E. length(Rows$RowNumbers) > 0
  #PRINT STOCKS THAT WERE REMOVED, OR PRINT THAT NO STOCKS WERE REMOVED.
  if (length(Rows$RowNumbers) > 0) {
    ReturnsClean = Returns[-Rows$RowNumbers,]
    print(paste("The following Stocks were removed:", paste(Rows$Stocks, collapse = " ")))
  } else {
    ReturnsClean = Returns
    print("No Stocks Were Removed") 
  }
  
  
  #REMOVE COLUMNS IF NECESSARY
  if (length(Columns$ColumnNumbers) > 0) {
    ReturnsClean = ReturnsClean[,-Columns$ColumnNumbers]
    print(paste("The following dates were removed:", paste(Columns$Dates, collapse = " ")))
  } else {
    print("No Dates Were Removed")
  }
  
  
  #MAKE SURE VOLUME, RETURNS HAVE SAME DAYS/INSTRUMENTS
  VolumeClean = Volume[
    which(rownames(Volume) %in% rownames(ReturnsClean)), #Rows 
    which(colnames(Volume) %in% colnames(ReturnsClean))  #Columns
  ]
  
  #SAFETY CHECK
  if ( any(rownames(VolumeClean) != rownames(ReturnsClean)) | any(colnames(VolumeClean) != colnames(ReturnsClean)) ) {
    stop("Row or Column names don't match")
  }

  return(list(ReturnsClean = ReturnsClean,
              VolumesClean = VolumeClean))
  
}














###TO ENSURE WE DON'T DIVIDE BY ZERO, WE REMOVE ALL STOCKS THAT HAD AT LEAST ONE DAY WITH NO VOLUME
RemoveStockWithZeroVolume <- function(Returns, Volume) {
  StockWithZeroVolume = which(apply(Volume, 1, function(row) any(row == 0)))
  
  if (length(StockWithZeroVolume) == 0) { #No stocks to be deleted
    print("No stocks were deleted")
    return(list(
      Returns = Returns,
      Volume = Volume
    ))
  }
  
  
  ReturnsClean = Returns[-StockWithZeroVolume,]
  VolumeClean = Volume[-StockWithZeroVolume,]
  
  print(paste("The following stocks were removed:", paste(rownames(Returns)[StockWithZeroVolume], collapse = " ")))
  paste("Totally", paste(length(StockWithZeroVolume)), "stocks removed")
  
  
  return(list(
    Returns = ReturnsClean,
    Volume = VolumeClean
  ))
}
  

#######################################
###########DATA PREPARATION############
#######################################


#CREATE RETURN MATRIX
Returns = ConstructReturnMatrix(Close, 1)

#CLEAN DATA
CleanData = SanityCheck(
  Volume = Volume,
  Returns = Returns,
  PropStocksZero = 0.1,
  PropDaysZero = 0.5,
  PropDaysUpperGrowth = 0.1,
  UpperBoundGrowth = 1)


Returns = CleanData$ReturnsClean
Volume = CleanData$VolumesClean



#REMOVE STOCKS WITH ZERO VOLUME
CleanData = RemoveStockWithZeroVolume(Returns, Volume)
Returns  = CleanData$Returns
Volume = CleanData$Volume

###THERE REMAINS 659 STOCKS OVER 4932 DAYS.

