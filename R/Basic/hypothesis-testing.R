PnL1 = Scores.CT.CCA$Regular$PnL[4,]
PnL2 = Scores.CT.CCA$Regular$PnL[3,]
PnL3 = Scores.CT.CCA$Regular$PnL[2,]



#Test For Difference in two streams of PnL
HyothesisTestPair <- function(PnL1, PnL2) {
  Test = sr_equality_test(X = cbind(PnL1, PnL2, PnL3))
  pvalue = Test$p.value
  return(pvalue)
}


HypothesisTest <- function(Scores.List) {
  k = length(Scores.List) #nr of methods
  
  Result = diag(k)
  for (i in 2:k) { #loop row
    for (j in 1:(i-1)) {
      browser()
      PnL1 = Scores.List[i]$Regular$PnL[4,]
      PnL2 = Scores.List[j]$Regular$PnL[4,]
      Result[i,j] = HyothesisTestPair(PnL1, PnL2)
    }
  }
  return(Result)
}

HypothesisTest(S)
