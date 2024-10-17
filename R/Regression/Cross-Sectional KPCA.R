library(kernlab)
library(parallel)



# PERFORMS STANDARD CROSS SECTIONAL REGRESSION ON ONE DAY, ALTOUGH THE PCA IS THROUGH A KERNEL
# KERNEL: THE NAME OF THE KERNEL (MUST BE COMPATIBLE WITH KPCA@KERNLAB)
# KPAR: PARAMETERS FOR KERNEL (MUST BE IN A LIST, MUST BE COMPATIBLE WITH KPCA@KERNLAB)
CrossSectional.KPCA.day <- function(Returns, t, H, nr_pc, kernel, kpar) {
  # EXTRACT LAST H DAYS OF HISTORY
  Returns <- Returns[, (t - H):(t - 1)]

  # STANDARDISE COLUMNS PRIOR TO USING KPCA
  Returns <- apply(Returns, 2, scale)

  # PERFORM KPCA
  s <- kpca(Returns,
    features = nr_pc,
    kernel = kernel, kpar = kpar
  )

  # EXTRACT PROJECTED DATA
  X <- S@rotated

  # STANDARDISE ROWS
  X <- X / apply(Returns, 1, sd)

  # PERFORM CROSS-SECTIONAL REJECTION
  y <- Returns[, ncol(Returns)] # last days of returns
  model <- lm(y ~ X)

  # EXTRACT RESIDUALS AND FORM PREDICTIONS
  red <- -model$residuals

  # RETURN
  return(Pred)
}


# PERFORM KPCA CS REGRESSION OVER AN INTERVAL [START, END]
CrossSectionRegression.KPCA <- function(Returns, start, end, h, nr_pc, kernel, kpar) {
  # VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  global_var_list <- c("CrossSectional.KPCA.day", "constructRho")

  # VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  local_var_list <- c("Returns", "h", "nr_pc", "kernel", "kpar")

  cl <- snow::makeCluster(detectCores() - 1)
  clusterCall(cl, function() library("kernlab"))
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir = environment())


  predictions <- snow::parSapply(cl, start:end, function(t) {
    CrossSectional.KPCA.day(
      Returns = Returns,
      t = t,
      h = H, nr_pc = nr_pc,
      kernel = kernel, kpar = kpar
    )
  })

  snow::stopCluster(cl)

  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(Returns)

  return(predictions)
}
