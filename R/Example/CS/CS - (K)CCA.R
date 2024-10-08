# 
# Predictions.CS.CCA=CrossSectionRegression.CCA(Returns = Returns, Volume = Volume,
#                                               Start = 500, End = ncol(Returns),
#                                               H=100, HV = 100,
#                                               NrC.R = 15, NrC.V = 5,
#                                               d = 20)
# 
# 
# write.csv(Predictions.CS.CCA, "./Results/Predictions/CS CCA/Predictions.CS.CCA.csv")



Predictions.CS.CCA = read.csv("./Results/Predictions/CS CCA/Predictions.CS.CCA.csv", header = T)
rownames(Predictions.CS.CCA) = Predictions.CS.CCA[, 1]
Predictions.CS.CCA = Predictions.CS.CCA[, -1]
colnames(Predictions.CS.CCA) = 500:4932



Scores.CS.CCA = performFullAnalysis(Returns = Returns,
                         Predictions = Predictions.CS.CCA,
                         Q=(1:4)/4, r = 30)



####################################################################################



# 
# Predictions.CS.KCCA=CrossSectionRegression.KCCA(Returns = Returns, Volume = Volume,
#                                                 Start = 500, End = ncol(Returns),
#                                                 H=100, HV = 100,
#                                                 NrC.R = 15, NrC.V = 5,
#                                                 d = 20)
# 
# write.csv(Predictions.CS.KCCA, "./Results/Predictions/CS CCA/Predictions.CS.KCCA.csv")



Predictions.CS.KCCA = read.csv("./Results/Predictions/CS CCA/Predictions.CS.KCCA.csv", header = T)
rownames(Predictions.CS.KCCA) = Predictions.CS.KCCA[, 1]
Predictions.CS.KCCA = Predictions.CS.KCCA[, -1]
colnames(Predictions.CS.KCCA) = 500:4932

Scores.CS.KCCA = performFullAnalysis(Returns = Returns,
                         Predictions = Predictions.CS.KCCA,
                         Q=(1:4)/4, r = 30)






SharpePPT.CS.KCCA = createSharpePPTPlot(Scores =list(Scores.CS, Scores.CS.CCA, Scores.CS.KCCA),
                                         Labels = c("CS", "CS CCA", "CS KCCA"),
                                         BaseModels = 1,
                                         Type = "CS")



ggsave(filename = "SharpePPT.CS.KCCA.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.CS.KCCA,
       width = 5, height = 5)



CumSum.CS.KCCA = createCumSumPnLPlot(Scores =list(Scores.CS, Scores.CS.CCA, Scores.CS.KCCA),
                                      Labels = c("CS", "CS CCA", "CS KCCA"),
                                      BaseModels = 1,
                                      Type = "CS")

ggsave(filename = "CumSum.CS.KCCA.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.CS.KCCA,
       width = 5, height = 6)





