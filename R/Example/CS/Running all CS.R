##################### BASE#############################

# Prediction.CS= CrossSectionalRegression(Returns = Returns,
# start = 500,
# end = ncol(Returns),
# H = 252,
# nr_pc = 20)
# write.csv(Prediction.CS, "./Results/Predictions/Base/Prediction.CS.csv")





#
# map_list = list(f = function(x) x,
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
# map_list = list(f = function(x) x,
#                 g = log,
#                 h= function(x) x^(1/3),
#                 i = sqrt)
#
#
#
# Predictions.CS.VW.Divide <- crossSectionalRegressionMappedVW(Returns,
#                                                               Volume,
#                                                               start = 500, end = ncol(Returns),
#                                                               H = 252, nr_pc=20, d = 20,
#                                                               divide = TRUE,
#                                                               map_list)
#
# write.csv(Predictions.CS.VW.Divide, "./Results/Predictions/CS VW/Predictions.CS.VW.Divide.csv")
#
#
#
# Predictions.CS.VW.Multiply <- crossSectionalRegressionMappedVW(Returns,
#                                                               Volume,
#                                                               start = 500, end = ncol(Returns),
#                                                               H = 252, nr_pc=20, d = 20,
#                                                               divide = FALSE,
#                                                               map_list)
#
# write.csv(Predictions.CS.VW.Multiply, "./Results/Predictions/CS VW/Predictions.CS.VW.Multiply.csv")
#



############################ (K)CCA #################################################



# Predictions.CS.CCA=CrossSectionalRegression.CCA(Returns = Returns, Volume = Volume,
#                                    start = 500, end = ncol(Returns),
#                                    H=100, HV = 100,
#                                    nr_c_r = 15, nr_c_v = 5,
#                                    d = 20)
#
#
# write.csv(Predictions.CS.CCA, "./Results/Predictions/CS CCA/Predictions.CS.CCA.csv")


# Predictions.CS.KCCA=CrossSectionalRegression.KCCA(Returns = Returns, Volume = Volume,
#                                       start = 500, end = ncol(Returns),
#                                       H=100, HV = 100,
#                                       nr_c_r = 15, nr_c_v = 5,
#                                       d = 20)
#
# write.csv(Predictions.CS.KCCA, "./Results/Predictions/CS CCA/Predictions.CS.KCCA.csv")



#################### OVERTRADED ######################################


# Prediction.CS.Overtraded = crossSectionalRegressionOverTraded(start = 500, end = ncol(Returns),
#                                                             Volume, Returns,
#                                                             H = 252, nr_pc.V = 25,
#                                                             alpha = 175, nr_pc = 20)
#
# write.csv(Prediction.CS.Overtraded, "./Results/Predictions/CS OverTrading/Prediction.CS.Overtraded.csv")
#
