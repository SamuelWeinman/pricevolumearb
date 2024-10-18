################################
##### CLUSTER ONLY BY RETURN#####
################################




# Predictions.CS.Cluster.Return <- crossSectionalRegressionClustering(Returns = Returns,
#                                                         start = 500, end = ncol(Returns),
#                                                         H = 252, nr_pc = 20,
#                                                         k = 4,
#                                                         MinSize = 50,
#                                                         alpha = 1)

# write.csv(Predictions.CS.Cluster$Predictions, "Predictions.CS.Cluster.csv")
# write.csv(Predictions.CS.Cluster$k, "NrClusters Predictions.CS.Cluster.csv")


Predictions.CS.Cluster.Return <- list()




Predictions.CS.Cluster.Return$Predictions <- read.csv("./Results/Predictions/Clustering/Predictions.CS.Cluster.Return.csv")


rownames(Predictions.CS.Cluster.Return$Predictions) <- Predictions.CS.Cluster.Return$Predictions[, 1]
Predictions.CS.Cluster.Return$Predictions <- Predictions.CS.Cluster.Return$Predictions[, -1]
colnames(Predictions.CS.Cluster.Return$Predictions) <- 500:(ncol(Predictions.CS.Cluster.Return$Predictions) + 499)

Predictions.CS.Cluster.Return$k <- read.csv("./Results/Predictions/Clustering/NrClusters Predictions.CS.Cluster.Return.csv")
Predictions.CS.Cluster.Return$k <- Predictions.CS.Cluster.Return$k[, -1]



Scores.CS.Cluster.Return <- performFullAnalysis(
  Returns = Returns,
  Predictions = Predictions.CS.Cluster.Return$Predictions,
  Q = (1:4) / 4, r = 30
)




###################################
##### CLUSTER BY RETURN&VOLUME #####
###################################




# Predictions.CS.Cluster.Weighted <- crossSectionalRegressionClustering(Returns = Returns,
#                                                                  Volume = Volume,
# start = 500,
# end = ncol(Returns),
# H = 252, nr_pc = 20,
# k = 4,
# MinSize = 50,
# alpha = 0.5)

# write.csv(Predictions.CS.Cluster.Weighted$Predictions, "Predictions.CS.Cluster.Weighted.csv")
# write.csv(Predictions.CS.Cluster.Weighted$k, "NrClusters Predictions.CS.Cluster.Weighted.csv")


Predictions.CS.Cluster.Weighted <- list()

Predictions.CS.Cluster.Weighted$Predictions <- read.csv("./Results/Predictions/Clustering/Predictions.CS.Cluster.Weighted.csv")
rownames(Predictions.CS.Cluster.Weighted$Predictions) <- Predictions.CS.Cluster.Weighted$Predictions[, 1]
Predictions.CS.Cluster.Weighted$Predictions <- Predictions.CS.Cluster.Weighted$Predictions[, -1]
colnames(Predictions.CS.Cluster.Weighted$Predictions) <- 500:(ncol(Predictions.CS.Cluster.Weighted$Predictions) + 499)

Predictions.CS.Cluster.Weighted$k <- read.csv("./Results/Predictions/Clustering/NrClusters Predictions.CS.Cluster.Weighted.csv")
Predictions.CS.Cluster.Weighted$k <- Predictions.CS.Cluster.Weighted$k[, -1]



Scores.CS.Cluster.Weighted <- performFullAnalysis(
  Returns = Returns,
  Predictions = Predictions.CS.Cluster.Weighted$Predictions,
  Q = (1:4) / 4, r = 30
)




###################################
##### CLUSTER BY VOLUME ############
###################################

# Predictions.CS.Cluster.Volume <- crossSectionalRegressionClustering(Returns = Returns,
#                                                                   Volume = Volume,
#                                                                   start = 500,
#                                                                   end = ncol(Returns),
#                                                                   H = 252, nr_pc = 20,
#                                                                   k = 4,
#                                                                   MinSize = 50,
#                                                                   alpha = 0)

# write.csv(Predictions.CS.Cluster.Volume$Predictions, "Predictions.CS.Cluster.Volume.csv")
# write.csv(Predictions.CS.Cluster.Volume$k, "NrClusters Predictions.CS.Cluster.Volume.csv")




Predictions.CS.Cluster.Volume <- list()

Predictions.CS.Cluster.Volume$Predictions <- read.csv("./Results/Predictions/Clustering/Predictions.CS.Cluster.Volume.csv")
rownames(Predictions.CS.Cluster.Volume$Predictions) <- Predictions.CS.Cluster.Volume$Predictions[, 1]
Predictions.CS.Cluster.Volume$Predictions <- Predictions.CS.Cluster.Volume$Predictions[, -1]
colnames(Predictions.CS.Cluster.Volume$Predictions) <- 500:(ncol(Predictions.CS.Cluster.Volume$Predictions) + 499)

Predictions.CS.Cluster.Volume$k <- read.csv("./Results/Predictions/Clustering/NrClusters Predictions.CS.Cluster.Volume.csv")
Predictions.CS.Cluster.Volume$k <- Predictions.CS.Cluster.Volume$k[, -1]



Scores.CS.Cluster.Volume <- performFullAnalysisperformFullAnalysis(
  Returns = Returns,
  Predictions = Predictions.CS.Cluster.Volume$Predictions,
  r = 30, Q = (1:4) / 4
)







################# PLOT#############

CumSum.Clustering <- createCumSumPnLPlot(
  Scores = list(
    Scores.CS,
    Scores.CS.Cluster.Return,
    Scores.CS.Cluster.Weighted,
    Scores.CS.Cluster.Volume
  ),
  Labels = c(
    "No Clustering",
    "Cluster Return",
    "Cluster Return & Volume",
    "Cluster Volume"
  ),
  Type = "CS",
  BaseModels = 1
)



SharpePPT.Clustering <- createSharpePPTPlot(
  Scores = list(
    Scores.CS,
    Scores.CS.Cluster.Return,
    Scores.CS.Cluster.Weighted,
    Scores.CS.Cluster.Volume
  ),
  Labels = c(
    "No Clustering",
    "Cluster Returns",
    "Cluster Returns & Volumes",
    "Cluster Volumes"
  ),
  Type = "CS",
  BaseModels = 1
)


### SAVE
ggsave(
  filename = "SharpePPT.Clustering.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = SharpePPT.Clustering,
  width = 6, height = 5
)



ggsave(
  filename = "CumSumPnL.Clustering.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = CumSum.Clustering,
  width = 5, height = 6
)






###### MARKET EXCESS


Scores.CS.Cluster.Return.ME <- performFullAnalysis(
  Returns = Returns,
  Predictions = Predictions.CS.Cluster.Return$Predictions,
  Q = (1:4) / 4, r = 30, SubtractSpy = TRUE
)

Scores.CS.Cluster.Weighted.ME <- performFullAnalysis(
  Returns = Returns,
  Predictions = Predictions.CS.Cluster.Weighted$Predictions,
  Q = (1:4) / 4, r = 30, SubtractSpy = TRUE
)

Scores.CS.Cluster.Volume.ME <- performFullAnalysis(
  Returns = Returns,
  Predictions = Predictions.CS.Cluster.Volume$Predictions,
  r = 30, Q = (1:4) / 4, SubtractSpy = TRUE
)




SharpePPT.Clustering.ME <- createSharpePPTPlot(
  Scores = list(
    Scores.CS.ME,
    Scores.CS.Cluster.Return.ME,
    Scores.CS.Cluster.Weighted.ME,
    Scores.CS.Cluster.Volume.ME
  ),
  Labels = c(
    "No Clustering",
    "Cluster Return",
    "Cluster Return & Volume",
    "Cluster Volume"
  ),
  Type = "CS",
  BaseModels = 1
)
