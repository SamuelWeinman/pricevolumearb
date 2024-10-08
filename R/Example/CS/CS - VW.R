#PREDICTIONS ALREADY DONE
#"CLEAN"

MAP.list = list(f = function(x) x,
                g = log,
                h= function(x) x^(1/3),
                i = sqrt)





############################ DIVIDE #################################################

# Predictions.CS.VW.Divide <- Outside_CrossSectionRegression.VW(Returns,
#                                                               Volume,
#                                                               Start = 500, End = ncol(Returns),
#                                                               H = 252, NrPC=20,d = 20,
#                                                               divide = TRUE,
#                                                               MAP.list)
# 




Predictions.CS.VW.Divide = read.csv("./Results/Predictions/CS VW/Predictions.CS.VW.Divide.csv", header = T)
rownames(Predictions.CS.VW.Divide) = Predictions.CS.VW.Divide[,1]
Predictions.CS.VW.Divide = Predictions.CS.VW.Divide[,-1]
colnames(Predictions.CS.VW.Divide) = rep(500:4932,4)
Predictions.CS.VW.Divide = list(Predictions.CS.VW.Divide[,1:4433],
                                Predictions.CS.VW.Divide[,4433+(1:4433)],
                                Predictions.CS.VW.Divide[,2*4433+(1:4433)],
                                Predictions.CS.VW.Divide[,3*4433+(1:4433)])





   
Scores.CS.VW.Divide = performFullAnalysisFromList(Returns, Predictions.List = Predictions.CS.VW.Divide,
                                    Q = (1:4/4), r = 30)




Scores.CS.VW.Divide.Combined = append(list(Scores.CS),Scores.CS.VW.Divide)
names(Scores.CS.VW.Divide.Combined) = c("Not weighted", "x", "log(x)",
                                        "x^1/3", "sqrt(x)")


CumSum.CS.VW.Divide= createCumSumPnLPlot(Scores = Scores.CS.VW.Divide.Combined, 
                                          Labels = c("Not Weighted (Baseline)",
                                                     "V",
                                                     "log(V)",
                                                     "V^(1/3)",
                                                     "sqrt(V)"),
                                          BaseModels = 1,
                                          Type = "CS")
         





SharpePPT.CS.VW.Divide= createSharpePPTPlot(Scores = Scores.CS.VW.Divide.Combined,
                                             Labels = c("Not Weighted (Baseline)",
                                                        "V",
                                                        "log(V)",
                                                        "V^(1/3)",
                                                        "sqrt(V)"),
                                             BaseModels = 1,
                                             Title = "Cross-Sectional Regression, Division by Volume",
                                             Type = "CS")




ggsave(filename = "CumSum.CS.VW.Divide.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.CS.VW.Divide,
       width = 5, height = 6)


ggsave(filename = "SharpePPT.CS.VW.Divide.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.CS.VW.Divide,
       width = 5, height = 5)






############################ Multiply  #################################################

# Predictions.CS.VW.Multiply<- Outside_CrossSectionRegression.VW(Returns, 
#                                                               Volume, 
#                                                               Start = 500, End = ncol(Returns),
#                                                               H = 252, NrPC=20,d = 20,
#                                                               divide = FALSE,
#                                                               MAP.list)



Predictions.CS.VW.Multiply = read.csv("./Results/Predictions/CS VW/Predictions.CS.VW.Multiply.csv", header = T)
rownames(Predictions.CS.VW.Multiply) = Predictions.CS.VW.Multiply[,1]
Predictions.CS.VW.Multiply = Predictions.CS.VW.Multiply[,-1]
colnames(Predictions.CS.VW.Multiply) = rep(500:4932,4)
Predictions.CS.VW.Multiply = list(Predictions.CS.VW.Multiply[,1:4433],
                                   Predictions.CS.VW.Multiply[,4433+(1:4433)],
                                   Predictions.CS.VW.Multiply[,2*4433+(1:4433)],
                                   Predictions.CS.VW.Multiply[,3*4433+(1:4433)])







Scores.CS.VW.Multiply = performFullAnalysisFromList(Returns, Predictions.List = Predictions.CS.VW.Multiply,
                                    Q = (1:4/4), r = 30)
Scores.CS.VW.Multiply.Combined = append(list(Scores.CS),Scores.CS.VW.Multiply)
names(Scores.CS.VW.Multiply.Combined) = c("Not weighted", "x", "log(x)",
                                        "x^1/3", "sqrt(x)")




CumSum.CS.VW.Multiply= createCumSumPnLPlot(Scores =Scores.CS.VW.Multiply.Combined, 
                                            Labels = c("Not Weighted (Baseline)",
                                                       "V",
                                                       "log(V)",
                                                       "V^(1/3)",
                                                       "sqrt(V)"),
                                            BaseModels = 1,
                                            Type = "CS")






SharpePPT.CS.VW.Multiply = createSharpePPTPlot(Scores =Scores.CS.VW.Multiply.Combined, 
                                                Labels = c("Not Weighted (Baseline)",
                                                           "V",
                                                           "log(V)",
                                                           "V^(1/3)",
                                                           "sqrt(V)"),
                                                BaseModels = 1,
                                                Title = "Cross-Sectional Regression, Multiplication by Volume",
                                                Type = "CS")


ggsave(filename = "CumSum.CS.VW.Multiply.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.CS.VW.Multiply,
       width = 5, height = 6)



ggsave(filename = "SharpePPT.CS.VW.Multiply.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.CS.VW.Multiply,
       width = 5, height = 5)












