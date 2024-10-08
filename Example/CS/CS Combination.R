Predictions.CS.Combined = CombinePrediction(PredictionStrong= Predictions.CS.Cluster.Weighted$Predictions,
                                            PredictionsWeak = Predictions.CS.VW.Divide[[1]], 
                                            alpha=0.75)



Scores.CS.Combined = Analysis(Returns=Returns,
                              Predictions=Predictions.CS.Combined,
                              Q=(1:4)/4, r=30)





SharpePPT.Combined = CreatePlot.SharpePPT(Scores = list(Scores.CS,
                                                        Scores.CS.Cluster.Weighted,
                                                        Scores.CS.VW.Divide[[1]],
                                                        Scores.CS.Combined),
                                            Labels = c("CS", 
                                                       "CS Clustering (returns & volumes)", 
                                                       "CS Volume Weighting",
                                                       "CS Combined"),
                                            Type="CS",
                                            BaseModels = 1)


###SAVE
ggsave(filename = "SharpePPT.Combined.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.Combined,
       width = 6.1, height = 5.2)




CumSum.Combined = CreatePlot.CumSumPnL(Scores = list(Scores.CS,
                                                        Scores.CS.Cluster.Weighted,
                                                        Scores.CS.VW.Divide[[1]],
                                                        Scores.CS.Combined),
                                          Labels = c("CS", 
                                                     "CS Cluster", 
                                                     "CS VW",
                                                     "CS Combined"),
                                          Type="CS",
                                          BaseModels = 1)


###SAVE
ggsave(filename = "Cumsum.Combined.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.Combined,
       width = 5, height = 6)




