####################OPTIMISE L###################

# CT.Scores.OverL = OptimiseL(Returns=Returns, 
#                             Start=500, End=ncol(Returns), 
#                             NrPC=20,H=252,
#                             L.Candidates=seq(25,60,by=5), bSensativity=0.01,
#                             Q=(1:4)/4,r=30)
# 
# write.csv(CT.Scores.OverL, "./Results/Predictions/Base/CT.Scores.OverL.csv")




####################BASE###################


# Prediction.CT = CTRegression(Returns = Returns,
#                             Start=500,
#                             End=ncol(Returns),
#                             NrPC=20,
#                             H=252,
#                             L=45,
#                             bSensativity=0.01)
# 
# write.csv(Prediction.CT, "./Results/Predictions/Base/Prediction.CT.csv")



############################ VW ###################################


# Predictions.CT.VW.Divide <- Outside_CTRegression.VW(Returns=Returns,
#                                                     Volume=Volume,
#                                                     Start=500, End=ncol(Returns),
#                                                     H=252, NrPC=20, L=45,
#                                                     bSensativity = 0.01, d=20,
#                                                     divide=TRUE, MAP.list=MAP.list)
#  
# write.csv(Predictions.CT.VW.Divide, "./Results/Predictions/CT VW/Predictions.CT.VW.Divide.csv")
# 
# 
# 
# Predictions.CT.VW.Multiply <- Outside_CTRegression.VW(Returns=Returns,
#                                                       Volume=Volume,
#                                                       Start=500, End=ncol(Returns),
#                                                       H=252, NrPC=20, L=45,
#                                                       bSensativity = 0.01, d=20,
#                                                       divide=FALSE, MAP.list=MAP.list)
# 
# write.csv(Predictions.CT.VW.Multiply, "./Results/Predictions/CT VW/Predictions.CT.VW.Multiply.csv")




#################### K(CCA) #################### 

# Predictions.CT.CCA=CTRegression.CCA(Returns=Returns, Volume=Volume,
#                                     Start=500, End=ncol(Returns),
#                                     H=100,HV=100, L=45,
#                                     NrC.R=15, NrC.V=5,
#                                     d=20,bSensativity=0.01)
# 
# write.csv(Predictions.CT.CCA, "./Results/Predictions/CT CCA/Predictions.CT.CCA.csv")
# 
# 
# Predictions.CT.KCCA=CTRegression.KCCA(Returns=Returns, Volume=Volume,
#                                       Start=500, End=ncol(Returns),
#                                       H=100,HV=100, L=45,
#                                       NrC.R=15, NrC.V=5,
#                                       d=20,bSensativity=0.01)
# 
# write.csv(Predictions.CT.KCCA, "./Results/Predictions/CT CCA/Predictions.CT.KCCA.csv")
# 


########## OVERTRADED ########## 



Predictions.CT.OverTraded = CTRegression.Overtrade(Volume, Returns,
                             Start=500, End=ncol(Returns),
                             H=252, NrPC.V=25, NrPC=20,
                             alpha=10,L=45,bSensativity=0.01)

write.csv(Predictions.CT.OverTraded, "./Results/Predictions/CT OverTrading/Predictions.CT.OverTraded.csv")

