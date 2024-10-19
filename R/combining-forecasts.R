# PREDICTIONS1: VECTOR OF N PREDICTIONS (ONE DAY)
# PREDICTIONS2: VECTOR OF N PREDICTIONS (SAME DAY)
# ALPHA: PROPORTION TO TAKE FROM PREDICTIONS1

## GIVEN TWO PREDICTIONS FOR THE SAME DAY, WE TAKE THE PROPRTION ALPHA STRONGEST PREDICTIONS FROM 1,
## AND THE OTHER ONE'S FROM PREDICTIONS2. BOTH VECTORS MUST CORRESPOND TO THE SAME DAY
## AND BE OF SAME LENGTH.
## IN THE COMBINED VECTOR, THE ONE'S EXTRACTED FROM PREDICTIONS1 WILL CORRESPOND TO THE ALPHA STRONGEST PREDICTIONS.
combineDailyPredictions <- function(prediction1, prediction2, alpha) {
  stocks1 <- abs(prediction1) > quantile(abs(prediction1), 1 - alpha)
  stocks2 <- !stocks1

  prediction <- numeric(length(prediction1))
  prediction[stocks2] <- prediction2[stocks2]
  prediction[stocks1] <- prediction1[stocks1] * max(abs(prediction2[stocks2])) / min(abs(prediction1[stocks1])) * 1.1

  return(prediction)
}



### TAKES TWO MATRICES (DATA FRAMES) OF DIMENSIONS NxT WHERE EACH COLUMN CORRESPONDS TO ONE DAY'S PREDICTION.
### THEN COMBINES THE MATREICES AS PER ABOVE.

# PREDICTIONSSTRONG: THE PREDICTIONS TO EXTRACT THE STRONGEST ALPHA PROPORTION FROM
# PREDICTIONSWEAK: WHAT TO BASE THE OTHER (1-ALPHA) PREDICTIONS ON

combinePrediction <- function(strong_predictions, weak_predictions, alpha) {
  combined_predictions <- sapply(1:ncol(strong_predictions), function(i) {
    combineDailyPredictions(
      prediction1 = strong_predictions[, i],
      prediction2 = weak_predictions[, i],
      alpha = alpha
    )
  })

  colnames(combined_predictions) <- colnames(strong_predictions)

  return(combined_predictions)
}
