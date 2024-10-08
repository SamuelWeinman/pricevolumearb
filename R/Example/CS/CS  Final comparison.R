SharpePPT.CS.Final = CreatePlot.SharpePPT(Scores = list(Scores.CS,
                                                        Scores.CS.Combined,
                                                        Scores.CS.full[[6]],
                                                        Scores.CS.Cluster.Weighted,
                                                        Scores.CS.VW.Divide[[1]],
                                                        Scores.CS.KCCA,
                                                        Scores.CS.Overtraded),
                                          Labels = c("CS", "CS Combined","CS KPCA","CS Weighted Clustering",
                                                     "CS VW","CS KCCA", "CS Decomposed Volume"),
                                          Type="CS",
                                          BaseModels = 1)


ggsave(filename = "SharpePPT.CS.Final.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.CS.Final,
       width = 6, height = 5)
