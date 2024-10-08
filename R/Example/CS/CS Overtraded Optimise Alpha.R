Alpha.Candidates = seq(40,250, by = 15)

# Scores = lapply(1:length(Alpha.Candidates), function(i){
#   print(i)
#   A = Alpha.Candidates[i]
#   Preds = CrossSectionRegression.OverTrade(Start = 500, End = ncol(Returns),
#                                                               Volume, Returns,
#                                                               H = 252, NrPC.V = 25,
#                                                               alpha = A, NrPC = 20)
#   S = Analysis(Returns, Preds, r = 30, Q = (1:4)/4)
#   return(S)
# })
# 
# write.csv(x = Scores, file= "./Results/Predictions/CS OverTrading/Scores.CS.OverTrading.OverAlpha.csv")




#EXTRACT MEAN AND VARIANCE


# Mean = sapply(1:length(Scores), function(i){
#   s = Scores[[i]]
#   mean(s$Regular$PnL)
# })
# 
# 
# sd = sapply(1:length(Scores), function(i){
#   s = Scores[[i]]
#   sd(s$Regular$PnL)
# })
# 
# 
# 
# write.csv(x = Mean, file= "./Results/Predictions/CS OverTrading/Scores.CS.OverTrading.OverAlpha.Means.csv")
# 
# write.csv(x = sd, file= "./Results/Predictions/CS OverTrading/Scores.CS.OverTrading.OverAlpha.sd.csv")


#CREATE PLOT

data = data.frame(alpha = Alpha.Candidates,
                  sd = sd,
                  Mean = Mean)


Plot.Mean = ggplot(data, aes(x = alpha, y = Mean))+ geom_point(color = "#8E0152") +
  ylab("Mean PnL")  


Plot.Sd = ggplot(data, aes(x = alpha, y = sd))+ geom_point(color = "#8E0152") +
  ylab("Standard Deviation PnL")


Plot.Sharpe = ggplot(data, aes(x = alpha, y = Mean/sd * sqrt(252)))+ geom_point(color = "#8E0152") +
  ylab("Sharpe Ratio")




Layout = matrix(c(1,3,2, 3), nrow = 2)
Plot = grid.arrange(Plot.Mean, Plot.Sd, Plot.Sharpe, layout_matrix = Layout)




ggsave(filename = "Performance.OverTrading.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = Plot)





Scores = read.csv("./Results/Predictions/CS OverTrading/Scores.CS.OverTrading.OverAlpha.csv")
colnames(Scores)[which(colnames(Scores) == "Regular.Sharpe")] = "Regular.Sharpe.0"
colnames(Scores)[which(colnames(Scores) == "Standard.Sharpe")] = "Standard.Sharpe.0"


S = list()
for (i in 1:15) {
  
  
  colname = paste("Regular.Sharpe.", toString(i-1), sep = "")
  col = which(colnames(Scores) == colname)
  SharpeRegular = unique(Scores[, col])
  PPTRegular = unique(Scores[, col+1])
  CumSumRegular = Scores[, (col+2):(col+5)]
  
  RegularList = list(PnL = numeric(0),
                     Sharpe = SharpeRegular,
                     PPT = PPTRegular,
                     PnL.Cumsum = CumSumRegular)
  
  
  
  colname = paste("Standard.Sharpe.", toString(i-1), sep = "")
  col = which(colnames(Scores) == colname)
  SharpeStandard = unique(Scores[, col])
  PPTstandard = unique(Scores[, col+1])
  CumSumStandard = Scores[, (col+2):(col+5)]
  
  StandardList = list(PnL = numeric(0),
                      Sharpe = SharpeStandard,
                      PPT = PPTstandard,
                      PnL.Cumsum = CumSumStandard)
  
  MethodList = list(Regular = RegularList,
                    Standard = StandardList)
  
  
  S = append(S, list(MethodList))
}

S.small = S[seq(1,15, by = 2)]

SharpePPT.OT.CS.OverAlpha = createSharpePPTPlot(Scores=S[seq(1,15, by = 2)],
                                       Labels=Alpha.Candidates[seq(1,15, by = 2)],
                                       Type = "CS",
                                       BaseModels = 1)


ggsave(filename = "SharpePPT.OT.CS.OverAlpha.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.OT.CS.OverAlpha,
       width = 5, height = 5)



