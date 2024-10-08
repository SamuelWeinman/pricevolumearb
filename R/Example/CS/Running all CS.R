#####################BASE#############################

#Prediction.CS= CrossSectionRegression(Returns = Returns,
                                      #Start = 500, 
                                      #End = ncol(Returns), 
                                      #H = 252,
                                      #NrPC = 20)
#write.csv(Prediction.CS, "./Results/Predictions/Base/Prediction.CS.csv")





# 
# MAP.list = list(f = function(x) x,
#                 g = log,
#                 h= function(x) x^(1/3),
#                 i = sqrt)
# 
# 
# 
# 
# 
# ############################ VW #################################################
# 
# MAP.list = list(f = function(x) x,
#                 g = log,
#                 h= function(x) x^(1/3),
#                 i = sqrt)
# 
# 
# 
# Predictions.CS.VW.Divide <- Outside_CrossSectionRegression.VW(Returns,
#                                                               Volume,
#                                                               Start = 500, End = ncol(Returns),
#                                                               H = 252, NrPC=20, d = 20,
#                                                               divide = TRUE,
#                                                               MAP.list)
# 
# write.csv(Predictions.CS.VW.Divide, "./Results/Predictions/CS VW/Predictions.CS.VW.Divide.csv")
# 
# 
# 
# Predictions.CS.VW.Multiply <- Outside_CrossSectionRegression.VW(Returns,
#                                                               Volume,
#                                                               Start = 500, End = ncol(Returns),
#                                                               H = 252, NrPC=20, d = 20,
#                                                               divide = FALSE,
#                                                               MAP.list)
# 
# write.csv(Predictions.CS.VW.Multiply, "./Results/Predictions/CS VW/Predictions.CS.VW.Multiply.csv")
# 



############################ (K)CCA #################################################



# Predictions.CS.CCA=CrossSectionRegression.CCA(Returns = Returns, Volume = Volume,
#                                    Start = 500, End = ncol(Returns),
#                                    H=100, HV = 100,
#                                    NrC.R = 15, NrC.V = 5,
#                                    d = 20)
# 
# 
# write.csv(Predictions.CS.CCA, "./Results/Predictions/CS CCA/Predictions.CS.CCA.csv")


# Predictions.CS.KCCA=CrossSectionRegression.KCCA(Returns = Returns, Volume = Volume,
#                                       Start = 500, End = ncol(Returns),
#                                       H=100, HV = 100,
#                                       NrC.R = 15, NrC.V = 5,
#                                       d = 20)
# 
# write.csv(Predictions.CS.KCCA, "./Results/Predictions/CS CCA/Predictions.CS.KCCA.csv")



#################### OVERTRADED ######################################


# Prediction.CS.Overtraded = CrossSectionRegression.OverTrade(Start = 500, End = ncol(Returns),
#                                                             Volume, Returns, 
#                                                             H = 252, NrPC.V = 25,
#                                                             alpha = 175, NrPC = 20)
# 
# write.csv(Prediction.CS.Overtraded, "./Results/Predictions/CS OverTrading/Prediction.CS.Overtraded.csv")
# 
