# PREDICTIONS1: VECTOR OF N PREDICTIONS (ONE DAY)
# PREDICTIONS2: VECTOR OF N PREDICTIONS (SAME DAY)
# ALPHA: PROPORTION TO TAKE FROM PREDICTIONS1

## GIVEN TWO PREDICTIONS FOR THE SAME DAY, WE TAKE THE PROPRTION ALPHA STRONGEST PREDICTIONS FROM 1,
## AND THE OTHER ONE'S FROM PREDICTIONS2. BOTH VECTORS MUST CORRESPOND TO THE SAME DAY
## AND BE OF SAME LENGTH.
## IN THE COMBINED VECTOR, THE ONE'S EXTRACTED FROM PREDICTIONS1 WILL CORRESPOND TO THE ALPHA STRONGEST PREDICTIONS.
combineDailyPredictions <- function(prediction1, prediction2, alpha) {
  # DEFINE WHAT STOCKS TO USE FROM PREDICTIONS1
  stocks1 <- abs(prediction1) > quantile(abs(prediction1), 1 - alpha)


  # DEFINE WHAT STOCKS TO USE FROM PREDICTIONS2
  stocks2 <- !stocks1

  # CREATE A NEW VECTOR AND TAKE THE STRONGEST PREDICTIONS FROM PREDICTION1
  prediction <- numeric(length(prediction1))
  prediction[stocks1] <- prediction1[stocks1]
  prediction[stocks2] <- prediction2[stocks2]


  # SCALE THE PREDICTIONS FROM FIRST PREDICTIONS SO THAT THE DECILE PORTFOLIOS ARE CONSTRUCTED FROM THESE
  # THE FOLLOWING ENSURES THAT THE MINIMUM ABSOLUTE VALUE FROM PREDICTIONS1 IS GREATER THAN THE MAXIMUM ABSOLUTE VALUE FROM PREDICTIONS2
  prediction[stocks1] <- prediction[stocks1] * max(abs(prediction[stocks2])) / min(abs(prediction[stocks1])) * 1.1

  # RETURN
  return(prediction)
}



### TAKES TWO MATRICES (DATA FRAMES) OF DIMENSIONS NxT WHERE EACH COLUMN CORRESPONDS TO ONE DAY'S PREDICTION.
### THEN COMBINES THE MATREICES AS PER ABOVE.

# PREDICTIONSSTRONG: THE PREDICTIONS TO EXTRACT THE STRONGEST ALPHA PROPORTION FROM
# PREDICTIONSWEAK: WHAT TO BASE THE OTHER (1-ALPHA) PREDICTIONS ON

combinePrediction <- function(strongPredictions, weakPredictions, alpha) {
  # LOOP THROUGH EACH COLUMN (DAY)
  # FOR EACH DAY, PERFORM combinePrediction.Day
  combinedPrediction <- sapply(1:ncol(strongPredictions), function(i) {
    combineDailyPredictionsy(
      prediction1 = strongPredictions[, i],
      prediction2 = weakPredictions[, i],
      alpha = alpha
    )
  })

  # CHANGE COLNAMES
  colnames(combinedPrediction) <- colnames(strongPredictions)

  # RETURN
  return(combinedPrediction)
}
