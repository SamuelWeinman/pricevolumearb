MAP.list = list(f = function(x) x,
                g = log,
                h= function(x) x^(1/3),
                i = sqrt)





############################ DIVIDE #################################################

# Predictions.CT.VW.Divide <- Outside_CTRegression.VW(Returns = Returns,
#                                                     Volume = Volume,
#                                                     Start = 500, End = ncol(Returns),
#                                                     H = 252, NrPC = 20, L = 45,
#                                                     bSensativity = 0.01, d = 20,
#                                                     divide = TRUE, MAP.list = MAP.list)
# 
# write.csv(Predictions.CT.VW.Divide, "./Results/Predictions/CT VW/Predictions.CT.VW.Divide.csv")




Predictions.CT.VW.Divide = read.csv("./Results/Predictions/CT VW/Predictions.CT.VW.Divide.csv", header = T)
rownames(Predictions.CT.VW.Divide) = Predictions.CT.VW.Divide[, 1]
Predictions.CT.VW.Divide = Predictions.CT.VW.Divide[, -1]
colnames(Predictions.CT.VW.Divide) = rep(500:4932, 4)
Predictions.CT.VW.Divide = list(Predictions.CT.VW.Divide[, 1:4433],
                                Predictions.CT.VW.Divide[, 4433+(1:4433)],
                                Predictions.CT.VW.Divide[, 2*4433+(1:4433)],
                                Predictions.CT.VW.Divide[, 3*4433+(1:4433)])




Scores.CT.VW.Divide = performFullAnalysisFromList(Returns, Predictions.List = Predictions.CT.VW.Divide,
                                    Q = (1:4/4), r = 30)

Scores.CT.VW.Divide.Combined = append(list(Scores.CT), Scores.CT.VW.Divide)
names(Scores.CT.VW.Divide.Combined) = c("Not weighted", "x", "log(x)",
                                        "x^1/3", "sqrt(x)")


CumSum.CT.VW.Divide= createCumSumPnLPlot(Scores = Scores.CT.VW.Divide.Combined, 
                                          Labels = c("Not Weighted (Baseline)",
                                                     "V",
                                                     "log(V)",
                                                     "V^(1/3)",
                                                     "sqrt(V)"),
                                          BaseModels = 1,
                                          Type = "CT")





SharpePPT.CT.VW.Divide= createSharpePPTPlot(Scores = Scores.CT.VW.Divide.Combined, 
                                             Labels = c("Not Weighted (Baseline)",
                                                        "V",
                                                        "log(V)",
                                                        "V^(1/3)",
                                                        "sqrt(V)"),
                                             BaseModels = 1,
                                             Title = "Cross-Temporal Regression, Division by Volume",
                                             Type = "CT")




ggsave(filename = "CumSum.CT.VW.Divide.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.CT.VW.Divide,
       width = 5, height = 6)



ggsave(filename = "SharpePPT.CT.VW.Divide.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.CT.VW.Divide,
       width = 5, height = 5)







############################ Multiply  #################################################

# Predictions.CT.VW.Multiply <- Outside_CTRegression.VW(Returns = Returns,
#                                                       Volume = Volume,
#                                                       Start = 500, End = ncol(Returns),
#                                                       H = 252, NrPC = 20, L = 45,
#                                                       bSensativity = 0.01, d = 20,
#                                                       divide = FALSE, MAP.list = MAP.list)

#write.csv(Predictions.CT.VW.Multiply, "./Results/Predictions/CT VW/Predictions.CT.VW.Multiply.csv")


Predictions.CT.VW.Multiply = read.csv("./Results/Predictions/CT VW/Predictions.CT.VW.Multiply.csv", header = T)
rownames(Predictions.CT.VW.Multiply) = Predictions.CT.VW.Multiply[, 1]
Predictions.CT.VW.Multiply = Predictions.CT.VW.Multiply[, -1]
colnames(Predictions.CT.VW.Multiply) = rep(500:4932, 4)
Predictions.CT.VW.Multiply = list(Predictions.CT.VW.Multiply[, 1:4433],
                                Predictions.CT.VW.Multiply[, 4433+(1:4433)],
                                Predictions.CT.VW.Multiply[, 2*4433+(1:4433)],
                                Predictions.CT.VW.Multiply[, 3*4433+(1:4433)])




Scores.CT.VW.Multiply = performFullAnalysisFromList(Returns, Predictions.List = Predictions.CT.VW.Multiply,
                                      Q = (1:4/4), r = 30)




Scores.CT.VW.Multiply.Combined = append(list(Scores.CT), Scores.CT.VW.Multiply)
names(Scores.CT.VW.Multiply.Combined) = c("Not weighted", "x", "log(x)",
                                        "x^1/3", "sqrt(x)")








CumSum.CT.VW.Multiply= createCumSumPnLPlot(Scores = Scores.CT.VW.Multiply.Combined, 
                                            Labels = c("Not Weighted (Baseline)",
                                                       "V",
                                                       "log(V)",
                                                       "V^(1/3)",
                                                       "sqrt(V)"),
                                            BaseModels = 1,
                                            Type = "CT")





SharpePPT.CT.VW.Multiply= createSharpePPTPlot(Scores = Scores.CT.VW.Multiply.Combined, 
                                               Labels = c("Not Weighted (Baseline)",
                                                          "V",
                                                          "log(V)",
                                                          "V^(1/3)",
                                                          "sqrt(V)"),
                                               Title = "Cross-Temporal Regression, Multiplication by Volume",
                                               BaseModels = 1,
                                               Type = "CT")



ggsave(filename = "CumSum.CT.VW.Multiply.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.CT.VW.Multiply,
       width = 5, height = 6)


ggsave(filename = "SharpePPT.CT.VW.Multiply.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.CT.VW.Multiply,
       width = 5, height = 5)














