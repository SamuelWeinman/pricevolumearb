# CONSTRUCT CLUSTERS OF TIME SERIES FROM CORRELATION MATRIX
# I.E. EACH STOCK IS MAPPED TO ONE OF K CLUSTERS
# RHO: CORRELATION MATRIX
# K: NR OF CLUSTERS
# min_size: MINUMUM NR OF MEMBERS PER CLUSTER, OR ELSE WILL USE FEWER CLUSTERS
constructClusters <- function(rho, k, min_size) {

  d <- as.dist(1 - rho)
  tree <- hclust(d)
  labels <- as.numeric(cutree(tree, k = k))


  updated_k <- k

  while (min(table(labels)) < min_size) {
      updated_k <- updated_k - 1
      labels <- as.numeric(cutree(tree, k = updated_k))
  }


  clusters <- lapply(1:updated_k, function(i) {which(labels == i)})
  return(clusters)
}


# PERFORMS CLUSTERING CROSS-SECTIONAL REGRESSION FOR ONE DAY (T)
# ALPHA: WEIGHTING; COR =  ALPHA * COR(RETURNS) + (1-ALPHA) COR(VOLUMES)
singleCrossSectionalRegressionClustering  <- function(returns, volume, t, h, nr_pc, k, min_size, alpha) {

  e <- extractEigenPortfolio(
    returns = returns[, (t - h):(t - 1)],
    nr_pc = nr_pc
  )
  rho <- alpha * constructRho(returns[, (t - h):(t - 1)]) + (1 - alpha) * constructRho(volume[, (t - h):(t - 1)])
  clusters <- constructClusters(rho = rho, k = k, min_size = min_size)

  predictions_list <- sapply(1:length(clusters), function(i) {
    index <- clusters[[i]]
    model <- lm(returns[index, t - 1] ~ e$portfolio[index, ])
    p <- -model$residuals
    return(p / sd(p))
  })

  if (k == length(clusters)) {
    predictions <- predictions_list
  } else {
    predictions <- numeric(nrow(returns))
    for (i in 1:k) {
      predictions[clusters[[i]]] <- predictions_list[[i]]
    }
  }

  return(list(
    predictions = predictions,
    k = k
  ))
}

crossSectionalRegressionClustering <- function(returns, volume, start, end, h, nr_pc, k, min_size, alpha) {

  global_var_list <- c(
    "singleCrossSectionalRegressionClustering ", "constructClusters",
    "extractEigenPortfolio", "constructEigenPortfolios",
    "constructRho"
  )

  local_var_list <- c("returns", "volume", "h", "nr_pc", "k", "min_size", "alpha")

  cl <- snow::makeCluster(detectCores() - 1)
  snow::clusterExport(cl, global_var_list)
  snow::clusterExport(cl, local_var_list, envir = environment())


  predictions <- snow::parSapply(cl, start:end, function(t) {
    singleCrossSectionalRegressionClustering(
      returns = returns,
      volume = volume,
      t = t, h = h,
      nr_pc = nr_pc,
      k = k, min_size = min_size,
      alpha = alpha
    )
  })

  snow::stopCluster(cl)

  predictions <- matrix(unlist(predictions[1, ]), ncol = end - start + 1)

  # CHANGE COL AND ROWNAMES AS APPROPRIATE.
  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  # RETURN
  return(list(
    predictions = predictions,
    k = k
  ))
}
