library(parallel)
# Prediction.CS.Overtraded = CrossSectionRegression.OverTrade(Start = 500, End = ncol(Returns),
#                                                             Volume, Returns,
#                                                             H = 252, nr_pc.V = 25,
#                                                             alpha = 175, nr_pc = 20)
#
# write.csv(Prediction.CS.Overtraded, "./Results/Predictions/CS OverTrading/Prediction.CS.Overtraded.csv")


Prediction.CS.Overtraded <- read.csv("./Results/Predictions/CS OverTrading/Prediction.CS.Overtraded.csv", header = T)
rownames(Prediction.CS.Overtraded) <- Prediction.CS.Overtraded[, 1]
Prediction.CS.Overtraded <- Prediction.CS.Overtraded[, -1]
colnames(Prediction.CS.Overtraded) <- 500:4932


Scores.CS.Overtraded <- performFullAnalysis(
  Returns = Returns,
  Predictions = Prediction.CS.Overtraded,
  Q = (1:4) / 4, r = 30
)


SharpePPT.CS.Overtraded <- createSharpePPTPlot(
  Scores = list(Scores.CS, Scores.CS.Overtraded),
  Labels = c("CS", "CS Decomposed Volume"),
  BaseModels = 1,
  Type = "CS"
)


ggsave(
  filename = "SharpePPT.CS.Overtraded.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = SharpePPT.CS.Overtraded,
  width = 5, height = 5
)


CumSum.CS.Overtraded <- createCumSumPnLPlot(
  Scores = list(Scores.CS, Scores.CS.Overtraded),
  Labels = c("CS", "CS Decomposed Volume"),
  BaseModels = 1,
  Type = "CS"
)


ggsave(
  filename = "CumSum.CS.Overtraded.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = CumSum.CS.Overtraded,
  width = 5, height = 6
)
