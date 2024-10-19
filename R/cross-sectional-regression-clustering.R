#' Construct Clusters Based on Hierarchical Clustering
#'
#' This function applies hierarchical clustering to construct k clusters based on a similarity matrix `rho` (e.g. correlation matrix). 
#' If the minimum size of any cluster is less than a specified `min_size`, the function will reduce the number of clusters (`k`) until this condition is met.
#'
#' @param rho A numeric matrix, the similarity matrix. It is often a correlation matrix.
#' @param k An integer, the desired number of clusters.
#' @param min_size An integer, the minimum desired size of the clusters.
#'
#' @return A list where each element is a vector of indices representing a cluster.
#'
#' @examples
#' #Example data
#' rho <- matrix(rnorm(16), 4, 4)
#' k <- 2
#' min_size <- 2
#' #Use the function
#' constructClusters(rho, k, min_size)
#' 
constructClusters <- function(rho, k, min_size) {
  d <- as.dist(1 - rho)
  tree <- hclust(d)
  labels <- as.numeric(cutree(tree, k = k))


  updated_k <- k

  while (min(table(labels)) < min_size) {
    updated_k <- updated_k - 1
    labels <- as.numeric(cutree(tree, k = updated_k))
  }


  clusters <- lapply(1:updated_k, function(i) {
    which(labels == i)
  })
  return(clusters)
}


#' Single Cross-Sectional Regression with Clustering
#'
#' This function performs cross-sectional regression on returns on a given day,
#' using recent historical data. Data points are clustered based on correlation and trading volume.
#' For each cluster, a linear model is fit between the returns of the previous day and the eigenportfolios. 
#' The negative residuals from these models are returned as predicted returns.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param t An integer, the day for which to perform regression.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc An integer, the number of principal components to extract.
#' @param k An integer, the desired number of clusters.
#' @param min_size An integer, the minimum desired size of the clusters.
#' @param alpha A numeric, the weight given to the correlation matrix in the combined similarity matrix used for clustering.
#'
#' @return A list with two elements:
#'         - A numeric vector of normalized predictions for returns on day `t`.
#'         - The final number of clusters.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(25), 5, 5)
#' volume <- matrix(rnorm(25), 5, 5)
#' t <- 3
#' h <- 2
#' nr_pc <- 2
#' k <- 2
#' min_size <- 2
#' alpha <- 0.5
#' #Use the function
#' singleCrossSectionalRegressionClustering(returns, volume, t, h, nr_pc, k, min_size, alpha)
#' 
singleCrossSectionalRegressionClustering <- function(returns, volume, t, h, nr_pc, k, min_size, alpha) {
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

#' Cross-Sectional Regression with Clustering
#'
#' This function performs cross-sectional regression on a range of days to predict returns, considering the effect of volume. 
#' Correlation-based distance is computed on the basis of returns and volume, and clustering is performed on this distance.
#' For each cluster, on each day in the range, a linear model is fit between the returns of the previous day and the eigen portfolios. 
#' The normalized negative residuals from these models are returned as predicted returns. The function is parallelized for increased speed.
#'
#' @param returns A numeric matrix, the returns data.
#' @param volume A numeric matrix, the volume data.
#' @param start An integer, the start of the day range for which to perform regression.
#' @param end An integer, the end of the day range for which to perform regression.
#' @param h An integer, the number of recent historical days to be used.
#' @param nr_pc An integer, the number of principal components to extract in eigen portfolios.
#' @param k An integer, the desired number of clusters.
#' @param min_size An integer, the minimum desired size of the clusters.
#' @param alpha A numeric, the weight given to the correlation matrix in the combined similarity matrix used for clustering.
#'
#' @return A list with two elements:
#'         - A numeric matrix of normalized predictions where each row corresponds to a day in the given `start:end` range and each column corresponds to a stock.
#'         - The final number of clusters.
#'
#' @examples
#' #Example data
#' returns <- matrix(rnorm(25), 5, 5)
#' volume <- matrix(rnorm(25), 5, 5)
#' start <- 1
#' end <- 4
#' h <- 2
#' nr_pc <- 2
#' k <- 2
#' min_size <- 1
#' alpha <- 0.5
#' #Use the function
#' crossSectionalRegressionClustering(returns, volume, start, end, h, nr_pc, k, min_size, alpha)
#' 
crossSectionalRegressionClustering <- function(returns, volume, start, end, h, nr_pc, k, min_size, alpha) {
  global_vars <- c(
    "singleCrossSectionalRegressionClustering ", "constructClusters",
    "extractEigenPortfolio", "constructEigenPortfolios",
    "constructRho"
  )

  local_vars <- c("returns", "volume", "h", "nr_pc", "k", "min_size", "alpha")

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  snow::clusterExport(cl, global_vars)
  snow::clusterExport(cl, local_vars, envir = environment())


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
  colnames(predictions) <- start:end
  rownames(predictions) <- rownames(returns)

  return(list(
    predictions = predictions,
    k = k
  ))
}
