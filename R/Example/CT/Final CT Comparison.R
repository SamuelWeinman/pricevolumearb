SharpePPT.CT.Final = createSharpePPTPlot(Scores = list(Scores.CT,
                                                        Scores.CT.Combined,
                                                        Scores.CT.full[[5]],
                                                        Scores.CT.VW.Divide[[1]],
                                                        Scores.CT.KCCA,
                                                        Scores.CT.OverTraded),
                                          Labels = c("CT", "CT Combined","CT KPCA", "CT VW","CT KCCA", "CT Decomposed Volume"),
                                          Type = "CT",
                                          BaseModels = 1)


ggsave(filename = "SharpePPT.CT.Final.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.CT.Final,
       width = 5, height = 5)



FinalPlot = grid.arrange(SharpePPT.CS.Final, SharpePPT.CT.Final, nrow = 1)


ggsave(filename = "FinalPlot.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = FinalPlot)
