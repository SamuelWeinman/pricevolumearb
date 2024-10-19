#' Combine Daily Predictions
#'
#' This function combines daily stock predictions. It calculates quantiles based on absolute values 
#' and updates predictions based on certain conditions for stocks.
#'
#' @param prediction1 Numeric vector, the first prediction.
#' @param prediction2 Numeric vector, the second prediction.
#' @param alpha Numeric value, the quantile threshold.
#' 
#' @return A numeric vector, the combined daily predictions.
#'
#' @examples
#' #Example data
#' pred1 <- c(0.2, 0.3, 0.1, 0.5, 0.4)
#' pred2 <- c(0.1, 0.1, 0.2, 0.1, 0.1)
#' alpha <- 0.8
#' #Use the function
#' combineDailyPredictions(pred1, pred2, alpha)
combineDailyPredictions <- function(prediction1, prediction2, alpha) {
  stocks1 <- abs(prediction1) > quantile(abs(prediction1), 1 - alpha)
  stocks2 <- !stocks1

  prediction <- numeric(length(prediction1))
  prediction[stocks2] <- prediction2[stocks2]
  prediction[stocks1] <- prediction1[stocks1] * max(abs(prediction2[stocks2])) / min(abs(prediction1[stocks1]))

  return(prediction)
}


#' Combine Predictions
#'
#' This function combines strong and weak stock predictions. It applies the 
#' `combineDailyPredictions` function for each column of the data.
#'
#' @param strong_predictions A numeric matrix, the strong predictions.
#' @param weak_predictions A numeric matrix, the weak predictions.
#' @param alpha Numeric value, the quantile threshold used in the 
#' `combineDailyPredictions` function.
#'
#' @return A numeric matrix, the combined predictions. The column names of the
#' returned matrix will be the same as the column names of the strong predictions.
#' 
#' @examples
#' #Example data
#' s_pred <- matrix(runif(25), 5, 5)
#' w_pred <- matrix(runif(25), 5, 5)
#' alpha <- 0.8
#' #Use the function
#' combinePrediction(s_pred, w_pred, alpha)
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
