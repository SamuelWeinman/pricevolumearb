


Kernel.List = c("rbfdot", "polydot", "tanhdot", "laplacedot", "anovadot")



Kpar.List = list(list(sigma = 0.2), #rbfdor
              list(degree = 8, scale = 2, offset = 1), #polydot
              list(scale = 2, offset = 1), #tanhdot
              list(sigma = 0.2), #laplace
              list(sigma = 0.2, degree =5)) #anovadot
                        





# 
# Scores.CS.KPCA = list()
# for (i in 1:length(Kernel.List)) {
#   print("Loop 1, Start kernel number:")
#   print(i)
#   Sys.time()
#   kernel = Kernel.List[i]
#   kpar = Kpar.List[[i]]
#   
#   Preds = CrossSectionRegression.KPCA(Returns = Returns, Start=500,End = ncol(Returns), 
#                                       H=252,NrPC = 20,
#                                       kernel = kernel, kpar = kpar)
#   
#   
#   Score = Analysis(Returns, Preds, Q = (1:4)/4, r = 30)
#   
#   Scores.CS.KPCA = append(Scores.CS.KPCA, list(Score))
# }
# 
# 
# 
# write.csv(Scores.CS.KPCA,  file= "./Results/Predictions/Scores.CS.KPCA.OverKernel.csv")



Scores.CS.KPCA = read.csv("./Results/Predictions/Scores.CS.KPCA.OverKernel.csv")
colnames(Scores.CS.KPCA)[which(colnames(Scores.CS.KPCA) == "Regular.Sharpe")] = "Regular.Sharpe.0"
colnames(Scores.CS.KPCA)[which(colnames(Scores.CS.KPCA) == "Standard.Sharpe")] = "Standard.Sharpe.0"


S = list()
for (i in 1:5) {
  

  colname = paste("Regular.Sharpe.", toString(i-1),sep = "")
  col = which(colnames(Scores.CS.KPCA) == colname)
  SharpeRegular = unique(Scores.CS.KPCA[,col])
  PPTRegular = unique(Scores.CS.KPCA[,col+1])
  CumSumRegular = Scores.CS.KPCA[,(col+2):(col+5)]
  
  RegularList = list(PnL = numeric(0),
                     Sharpe = SharpeRegular,
                     PPT = PPTRegular,
                     PnL.Cumsum = CumSumRegular)
  
  
  
  colname = paste("Standard.Sharpe.", toString(i-1),sep = "")
  col = which(colnames(Scores.CS.KPCA) == colname)
  SharpeStandard = unique(Scores.CS.KPCA[,col])
  PPTstandard = unique(Scores.CS.KPCA[,col+1])
  CumSumStandard = Scores.CS.KPCA[,(col+2):(col+5)]
  
  StandardList = list(PnL = numeric(0),
                     Sharpe = SharpeStandard,
                     PPT = PPTstandard,
                     PnL.Cumsum = CumSumStandard)
  
  MethodList = list(Regular = RegularList,
                    Standard = StandardList)
  
  
  S = append(S, list(MethodList))
}


Scores.CS.full = c(list(Scores.CS), S)
Labels = c("Linear", "Gaussian", "Polynomial","Hyperbolic Tangent","Laplacian","Anova")



plotSharpePPT = createSharpePPTPlot(Scores.CS.full, 
                            Labels = Labels,
                            Type = "CS",
                            BaseModels = 1,
                            LegendTitle = "Kernel:")

ggsave(filename = "SharpePPT.CS.KPCA.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = plotSharpePPT,
       width = 5, height = 5)


plotCumSum = createCumSumPnLPlot(Scores.CS.full, 
                                  Labels = Labels,
                                  Type = "CS",
                                  BaseModels = 1)


ggsave(filename = "CumSum.CS.KPCA.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = plotCumSum,
       width = 5, height = 6)
