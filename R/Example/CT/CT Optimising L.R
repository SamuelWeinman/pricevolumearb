#RETURNS THE SCORES (IN A LIST) FOR EACH VALUE OF L IN L.CANDIDATES
# L.CANDIDATES: ARRAY WITH CANDIDATES OF L, NEED L > NRPC
OptimiseL = function(Returns, Start, End, NrPC,H,L.Candidates, bSensativity,Q,r) {
  
  #CREATE LIST FOR STORING SCORES
  Scores = list()
  
  
  #LOOP THROUGH EACH CANDIDATE OF L, CAN CALCULATE CORRESPONDING SCORE
  for (Lcand in L.Candidates) {
    Pred = CTRegression(Returns, Start, End, NrPC,H,L=Lcand, bSensativity)
    Score = Analysis(Returns = Returns, Predictions = Pred, Q,r)
    Scores = append(Scores, list(Score))
  }
  
  #RETURN
  return(Scores)
}




# CT.Scores.OverL = OptimiseL(Returns=Returns, 
#                             Start=500, End=ncol(Returns), 
#                             NrPC=20,H=252,
#                             L.Candidates=seq(25,60,by=5), bSensativity=0.01,
#                             Q=(1:4)/4,r=30)
# 
# write.csv(CT.Scores.OverL, "./Results/Predictions/Base/CT.Scores.OverL.csv")


CT.Scores.OverL = read.csv("./Results/Predictions/Base/CT.Scores.OverL.csv", header=T)
colnames(CT.Scores.OverL)[which(colnames(CT.Scores.OverL) == "Regular.Sharpe")] = "Regular.Sharpe.0"
colnames(CT.Scores.OverL)[which(colnames(CT.Scores.OverL) == "Standard.Sharpe")] = "Standard.Sharpe.0"



S = list()
for (i in 1:8) {
  
  
  colname = paste("Regular.Sharpe.", toString(i-1),sep = "")
  col = which(colnames(CT.Scores.OverL) == colname)
  SharpeRegular = unique(CT.Scores.OverL[,col])
  PPTRegular = unique(CT.Scores.OverL[,col+1])
  CumSumRegular = CT.Scores.OverL[,(col+2):(col+5)]
  
  RegularList = list(PnL = numeric(0),
                     Sharpe = SharpeRegular,
                     PPT = PPTRegular,
                     PnL.Cumsum = CumSumRegular)
  
  
  
  colname = paste("Standard.Sharpe.", toString(i-1),sep = "")
  col = which(colnames(CT.Scores.OverL) == colname)
  SharpeStandard = unique(CT.Scores.OverL[,col])
  PPTstandard = unique(CT.Scores.OverL[,col+1])
  CumSumStandard = CT.Scores.OverL[,(col+2):(col+5)]
  
  StandardList = list(PnL = numeric(0),
                      Sharpe = SharpeStandard,
                      PPT = PPTstandard,
                      PnL.Cumsum = CumSumStandard)
  
  MethodList = list(Regular = RegularList,
                    Standard = StandardList)
  
  
  S = append(S, list(MethodList))
}






SharpePPT.OverL = CreatePlot.SharpePPT(Scores=S,
                                       Labels=seq(25,60,by=5),
                                       Type="CT",
                                       BaseModels = 1)




ggsave(filename = "SharpePPT.OverL.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.OverL,
       height = 5, width = 5)


CumSum.OverL = CreatePlot.CumSumPnL(Scores=CT.Scores.OverL,
                                       Labels=seq(25,60,by=5))

ggsave(filename = "CumSum.OverL.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.OverL)
                     