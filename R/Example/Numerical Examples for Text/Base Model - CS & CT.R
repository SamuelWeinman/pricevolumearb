####FOR NOW, USE READ FROM EXCEL RATHER THAN CREATE PREDICTION AGAIN###
####HOWEVER, ONLY CREATE PREDICTION SHOULD GO IN THE REPORT#####

###############################
#######CS APPROACH#############
###############################

#PREDICTION
#Prediction.CS= CrossSectionRegression(Returns = Returns,
                                               #Start = 500, 
                                               #End = ncol(Returns), 
                                               #H = 252,
                                               #NrPC = 20)
#write.csv(Prediction.CS,"./Results/Predictions/Base/Prediction.CS.csv")

Prediction.CS = read.csv("./Results/Predictions/Base/Prediction.CS.csv")
rownames(Prediction.CS) = Prediction.CS[,1]
Prediction.CS = Prediction.CS[,-1]
colnames(Prediction.CS) = 500:(ncol(Prediction.CS)+499)

#PERFORMANCE MEASURE
Scores.CS= performFullAnalysis(Returns = Returns,
                    Predictions = Prediction.CS, 
                    Q = (1:4)/4,
                    r = 30)


#############################
### CT REGRESSION APPROACH###
############################# 

#PREDICTION
# Prediction.CT = CTRegression(Returns = Returns,
#                             Start = 500,
#                             End = ncol(Returns),
#                             NrPC = 20,
#                             H = 252,
#                             L = 45,
#                             bSensativity = 0.01)
# 
# write.csv(Prediction.CT, "./Results/Predictions/Base/Prediction.CT.csv")

Prediction.CT = read.csv("./Results/Predictions/Base/Prediction.CT.csv")
rownames(Prediction.CT) = Prediction.CT[,1]
Prediction.CT = Prediction.CT[,-1]
colnames(Prediction.CT) = 500:(ncol(Prediction.CT)+499)


#PERFORMANCE MEASURES
Scores.CT = performFullAnalysis(Returns, Prediction.CT, Q = (1:4)/4, r = 30)




CumSum.Base= createCumSumPnLPlot(Scores = list(Scores.CS, Scores.CT),
                                  Labels = c("Cross-Sectional", "Cross-Temporal"),
                                  Type = "BASE")


SharpePPT.Base = createSharpePPTPlot(Scores = list(Scores.CS, Scores.CT), 
                                      BaseModels = 1:2,
                                      Labels = c("Cross-Sectional", "Cross-Temporal"),
                                      Type = "BASE")





###SAVE
ggsave(filename = "CumSum.Base.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.Base,
       width = 5, height = 6)


ggsave(filename = "SharpePPT.Base.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = SharpePPT.Base,
       width = 5, height = 5)






####### MARKET EXCESS
Scores.CS.ME = performFullAnalysis(Returns, Prediction.CS, Q = (1:4)/4, r = 30, SubtractSpy = TRUE)
Scores.CT.ME = performFullAnalysis(Returns, Prediction.CT, Q = (1:4)/4, r = 30, SubtractSpy = TRUE)

CumSum.Base.ME= createSharpePPTPlot(Scores = list(Scores.CS.ME, Scores.CT.ME),
                                  Labels = c("CS", "CT"),
                                  BaseModels = 1:2,
                                  Type = "BASE",
                                  Title = "Market Excess")


ggsave(filename = "CumSum.Base.ME.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = CumSum.Base.ME)
