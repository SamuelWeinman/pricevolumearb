# Predictions.CT.OverTraded = CTRegression.Overtrade(Volume, Returns,
#                              Start = 500, End = ncol(Returns),
#                              H = 252, NrPC.V = 25, NrPC = 20,
#                              alpha=100,L=45, bSensativity = 0.01)
# 
# write.csv(Predictions.CT.OverTraded, "./Results/Predictions/CT OverTrading/Predictions.CT.OverTraded.csv")



Predictions.CT.OverTraded = read.csv("./Results/Predictions/CT OverTrading/Predictions.CT.OverTraded.csv")
rownames(Predictions.CT.OverTraded) = Predictions.CT.OverTraded[, 1]
Predictions.CT.OverTraded = Predictions.CT.OverTraded[, -1]
colnames(Predictions.CT.OverTraded) = 500:(ncol(Predictions.CT.OverTraded)+499)


Scores.CT.OverTraded = performFullAnalysis(Returns = Returns,
                                Predictions = Predictions.CT.OverTraded,
                                r = 30, Q = (1:4)/4)


SharpePPT.CT.Overtraded = createSharpePPTPlot(Scores =list(Scores.CT, Scores.CT.OverTraded),
                                               Labels = c("CT", "CT Decomposed Volume"),
                                               BaseModels = 1,
                                               Type = "CT")


ggsave(filename = "SharpePPT.CT.Overtraded.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.CT.Overtraded,
       width = 5, height = 5)



CumSum.CT.Overtraded = createCumSumPnLPlot(Scores =list(Scores.CT, Scores.CT.OverTraded),
                                            Labels = c("CT", "CT Decomposed Volume"),
                                            BaseModels = 1,
                                            Type = "CT")


ggsave(filename = "CumSum.CT.Overtraded.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.CT.Overtraded,
       width = 5, height = 6)


