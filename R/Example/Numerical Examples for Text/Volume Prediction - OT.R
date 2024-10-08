H = 252
t = 4348
NrGroups = 7


col = which(colnames(Prediction.CS) == t)


Pred.OT.CS = unlist(Prediction.CS.Overtraded[,col])
Pred.OT.CT = unlist(Predictions.CT.OverTraded[,col])


#CONSTRUCT "EIGENPORTFOLIO" OF VOLUME
E.V = ExtractEigenPortfolio(StandardVolume, NrPC.V)

#CALCULATE BY HOW MUCH IT'S STOCK IS OVERTRADED OVERTRADING
#HERE Overtraded_{I,T} IS THE AMOUNT THAT STOCK I WAS OVERTRADED ON DAY T

model = lm(StandardVolume[,H]~E.V$EigenPortfolio)
Overtraded = model$residuals


Group = as.factor(ceiling(order(Overtraded) / ceiling(659/NrGroups)))




data = data.frame(Group = 1:7,
                  Magnitude.OT.CS = tapply(abs(Pred.OT.CS), Group, mean),
                  Magnitude.OT.CT = tapply(abs(Pred.OT.CT), Group, mean))



plot1 = ggplot(data, aes(x = Group, y = Magnitude.OT.CS)) +
  geom_point(col = "red") +
  scale_x_continuous(breaks = 1:7, labels = 1:7) +
  xlab("Group") + ylab("Average Magnitude") +
  ggtitle("Cross-Sectional Regression")


plot2 = ggplot(data, aes(x = Group, y = Magnitude.OT.CT)) +
  geom_point(col = "blue") +
  scale_x_continuous(breaks = 1:7, labels = 1:7) +
  xlab("Group") + ylab("Average Magnitude") +
  ggtitle("Cross-Temporal Regression")


plot = grid.arrange(plot1, plot2)

ggsave(filename = "plotVolume.OT.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = plot,
       height = 5, width = 5)


