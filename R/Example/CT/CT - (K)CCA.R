
# Predictions.CT.CCA=CTRegression.CCA(Returns = Returns, Volume = Volume,
#                                    Start = 500, End = ncol(Returns),
#                                    H=100, HV = 100, L = 45,
#                                    NrC.R = 15, NrC.V = 5,
#                                    d=20, bSensativity = 0.01)
# 
#write.csv(Predictions.CT.CCA, "./Results/Predictions/CT CCA/Predictions.CT.CCA.csv")


Predictions.CT.CCA = read.csv("./Results/Predictions/CT CCA/Predictions.CT.CCA.csv")
rownames(Predictions.CT.CCA) = Predictions.CT.CCA[, 1]
Predictions.CT.CCA = Predictions.CT.CCA[, -1]
colnames(Predictions.CT.CCA) = 500:(ncol(Predictions.CT.CCA)+499)

Scores.CT.CCA = performFullAnalysis(Returns, Predictions.CT.CCA, Q = (1:4)/4, r = 30)


# Predictions.CT.KCCA=CTRegression.KCCA(Returns = Returns, Volume = Volume,
#                                       Start = 500, End = ncol(Returns),
#                                       H=100, HV = 100, L = 45,
#                                       NrC.R = 15, NrC.V = 5,
#                                       d=20, bSensativity = 0.01)
# 
# write.csv(Predictions.CT.KCCA, "./Results/Predictions/CT CCA/Predictions.CT.KCCA.csv")


Predictions.CT.KCCA = read.csv("./Results/Predictions/CT CCA/Predictions.CT.KCCA.csv")
rownames(Predictions.CT.KCCA) = Predictions.CT.KCCA[, 1]
Predictions.CT.KCCA = Predictions.CT.KCCA[, -1]
colnames(Predictions.CT.KCCA) = 500:(ncol(Predictions.CT.KCCA)+499)

Scores.CT.KCCA = performFullAnalysis(Returns, Predictions.CT.KCCA, Q = (1:4)/4, r = 30)





##########################################################################

CumSum.CT.KCCA= createCumSumPnLPlot(Scores = list(Scores.CT, Scores.CT.CCA, Scores.CT.KCCA),
                                     Labels = c("CT", "CT CCA", "CT KCCA"),
                                     BaseModels = 1,
                                     Type = "CT")


SharpePPT.CT.KCCA = createSharpePPTPlot(Scores = list(Scores.CT, Scores.CT.CCA, Scores.CT.KCCA),
                                         Labels = c("CT", "CT CCA", "CT KCCA"),
                                         BaseModels = 1,
                                         Type = "CT")



ggsave(filename = "CumSum.CT.KCCA.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.CT.KCCA,
       width = 5, height = 6)




ggsave(filename = "SharpePPT.CT.KCCA.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.CT.KCCA,
       width = 5, height = 5)









