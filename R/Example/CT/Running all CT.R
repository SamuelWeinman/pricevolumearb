#################### OPTIMISE L###################

# CT.Scores.OverL = OptimiseL(Returns = Returns,
#                             start = 500, end = ncol(Returns),
#                             nr_pc=20, H = 252,
#                             L.Candidates=seq(25,60, by = 5), b_sensitivity = 0.01,
#                             Q=(1:4)/4, r = 30)
#
# write.csv(CT.Scores.OverL, "./Results/Predictions/Base/CT.Scores.OverL.csv")




#################### BASE###################


# Prediction.CT = CTRegression(Returns = Returns,
#                             start = 500,
#                             end = ncol(Returns),
#                             nr_pc = 20,
#                             H = 252,
#                             L = 45,
#                             b_sensitivity = 0.01)
#
# write.csv(Prediction.CT, "./Results/Predictions/Base/Prediction.CT.csv")



############################ VW ###################################


# Predictions.CT.VW.Divide <- Outside_crossTemporalRegressionWithVW(Returns = Returns,
#                                                     Volume = Volume,
#                                                     start = 500, end = ncol(Returns),
#                                                     H = 252, nr_pc = 20, L = 45,
#                                                     b_sensitivity = 0.01, d = 20,
#                                                     divide = TRUE, MAP.list = MAP.list)
#
# write.csv(Predictions.CT.VW.Divide, "./Results/Predictions/CT VW/Predictions.CT.VW.Divide.csv")
#
#
#
# Predictions.CT.VW.Multiply <- Outside_crossTemporalRegressionWithVW(Returns = Returns,
#                                                       Volume = Volume,
#                                                       start = 500, end = ncol(Returns),
#                                                       H = 252, nr_pc = 20, L = 45,
#                                                       b_sensitivity = 0.01, d = 20,
#                                                       divide = FALSE, MAP.list = MAP.list)
#
# write.csv(Predictions.CT.VW.Multiply, "./Results/Predictions/CT VW/Predictions.CT.VW.Multiply.csv")




#################### K(CCA) ####################

# Predictions.CT.CCA=crossTemporalRegressionCCA(Returns = Returns, Volume = Volume,
#                                     start = 500, end = ncol(Returns),
#                                     H=100, HV = 100, L = 45,
#                                     nr_c_r = 15, nr_c_v = 5,
#                                     d=20, b_sensitivity = 0.01)
#
# write.csv(Predictions.CT.CCA, "./Results/Predictions/CT CCA/Predictions.CT.CCA.csv")
#
#
# Predictions.CT.KCCA=CTRegression.KCCA(Returns = Returns, Volume = Volume,
#                                       start = 500, end = ncol(Returns),
#                                       H=100, HV = 100, L = 45,
#                                       nr_c_r = 15, nr_c_v = 5,
#                                       d=20, b_sensitivity = 0.01)
#
# write.csv(Predictions.CT.KCCA, "./Results/Predictions/CT CCA/Predictions.CT.KCCA.csv")
#


########## OVERTRADED ##########



Predictions.CT.OverTraded <- crossTemporalRegressionOvertrade(Volume, Returns,
  start = 500, end = ncol(Returns),
  H = 252, nr_pc.V = 25, nr_pc = 20,
  alpha = 10, L = 45, b_sensitivity = 0.01
)

write.csv(Predictions.CT.OverTraded, "./Results/Predictions/CT OverTrading/Predictions.CT.OverTraded.csv")
