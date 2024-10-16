library(kernlab)


###PERFORM KCCA ON THE SPACE OF VOLUMES & RETURNS
###THEN PERFORMS CROSS-TEMPORAL REGRESSION USING THE EMBEDDINGS OF RETURNS AND VOLUMES
#VOLUME SHOULD ALREADY BE STANDARDISED!!
#PARAMETERS AS FOLLOWS:
# T: WHAT DAY WE ARE FORECASTING
# H: NR OF DAYS OF RETURN TO USE FOR EMBEDDING
# hv: NR OF DAYS OF VOLUME TO USE FOR EMBEDDING
# L: NR OF DAYS TO USE FOR REGRESSION ON EIGENRETURNS
# NRC.R: HOW MANY FEAURES OF RETURN EMBEDDINGS TO USE
# NRC.V: HOW MANY FEAURES OF VOLUME EMBEDDINGS TO USE
# b_sensitivity: HOW CLOSE REGRESSION HAS TO BE TO ONE IN ORDER TO REJECT MEAN-REVERSION

singleCrossTemporalRegressionWithKCCA = function(returns, volume, t, h, hv, l, nr_c_r, nr_c_v, b_sensitivity) {
  
  x = as.matrix(returns[, (t-h):(t-1)])
  y = as.matrix(volume[, (t-hv):(t-1)])
  
  c = kcca(x, y, ncomps = max(c(nr_c_r, nr_c_v)))
  
  x_coeff = c@xcoef[, 1:nr_c_r]
  y_coeff= c@ycoef[, 1:nr_c_v]
  
  x_coeff = x_coeff/apply(as.matrix(returns[,(t-h):(t-1)]),1, sd)
  y_coeff = y_coeff/apply(as.matrix(returns[,(t-h):(t-1)]),1, sd)

  portfolio = cbind(x_coeff, y_coeff)
  portfolio_returns = t(returns[(t-l):(t-1)]) %*% portfolio

  models <- lapply(1:nrow(returns), function(i) {
    y = unlist(returns[i, (t-l):(t-1)]) 
    return(lm(as.numeric(y)~portfolio_returns))
  })
  
  coefficients = estimateCoefficeients(models, b_sensitivity = b_sensitivity)
  
  s = numeric(nrow(returns))
  mean_reverting_indices = coefficients$is_mean_reverting == 1 #mean reversion 
  s[index] = -coefficients$m[mean_reverting_indices]/sqrt(coefficients$sigma_eq_squared[mean_reverting_indices])
  
  #RETURN
  return(list(
    s = s,
    is_mean_reverting = coefficients$is_mean_reverting))
}

#PERFORMS CT KCCA ON [START, END]
#D: HOW MANY DAYS TO STANDARDISE VOLUME OVER
crossTemporalRegressionWithKCCA <- function(returns, volume, start, end, h, hv, l, nr_c_r, nr_c_v, d, b_sensitivity) {

  standardisedVolume  =  volume / t(rolling_mean(t(as.matrix(volume)), width = d))
  
  globalvarlist = c("singleCrossTemporalRegressionWithKCCA", "estimateCoefficeients")  
  localvarlist = c("returns", "h", "hv","l", "nr_c_r", "nr_c_v","b_sensitivity", "standardisedVolume" )
    
  cl = snow::makeCluster(detectCores()-1)
  clusterCall(cl, function() library("kernlab"))
  clusterCall(cl, function() library("plyr"))
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment())
  
  predictions = snow::parSapply(cl, start:end, function(t) {
    s = singleCrossTemporalRegressionWithKCCA(returns = returns, volume = standardisedVolume, 
                            t = t, h = h, hv = hv,
                            L=L, nr_c_r = nr_c_r, nr_c_v = nr_c_v,
                            b_sensitivity = b_sensitivity)
    p = -s_scores
    return(p)
  })
  
  snow::stopCluster(cl)
  
  rownames(predictions) = rownames(returns)
  colnames(predictions) = start:end
  
  return(predictions)
}

