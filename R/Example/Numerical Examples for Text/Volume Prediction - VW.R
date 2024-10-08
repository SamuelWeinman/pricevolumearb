d = 20
t = 4348
NrGroups = 7

col = which(colnames(Prediction.CS) == t)


Pred.VW.CS = unlist(Predictions.CS.VW.Divide[[1]][, col])
Pred.VW.CT = unlist(Predictions.CT.VW.Divide[[1]][, col])


StandardVolume = unlist(Volume[, t-1] / apply(Volume[,(t-d):(t-1)],1, mean))

Group = as.factor(ceiling(order(StandardVolume) / ceiling(659/NrGroups)))


data = data.frame(Group = 1:7,
                  Magnitude.VW.CS = tapply(abs(Pred.VW.CS), Group, mean),
                  Magnitude.VW.CT = tapply(abs(Pred.VW.CT), Group, mean))




plotCS = ggplot(data, aes(x = Group, y = Magnitude.VW.CS)) +
  geom_point(col = "red") +
  scale_x_continuous(breaks = 1:7, labels = 1:7) +
  xlab("Group") + ylab("Average Magnitude") +
  ggtitle("Cross-Sectional Regression")

plotCT = ggplot(data, aes(x = Group, y = Magnitude.VW.CT)) +
  geom_point(col = "blue") +
  scale_x_continuous(breaks = 1:7, labels = 1:7) +
  xlab("Group") + ylab("Average Magnitude") +
  ggtitle("Cross-Temporal Regression")


plot = grid.arrange(plotCS, plotCT)

ggsave(filename = "plotVolume.VW.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = plot,
       height = 5, width = 5)

