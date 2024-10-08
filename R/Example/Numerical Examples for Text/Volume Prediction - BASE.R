d = 20
t = 4348
NrGroups = 7


col = which(colnames(Prediction.CS) == t)

Pred.CS = unlist(Prediction.CS[, col])
Pred.CT = unlist(Prediction.CT[, col])







StandardVolume = unlist(Volume[, t-1] / apply(Volume[,(t-d):(t-1)],1, mean))



Group = as.factor(ceiling(order(StandardVolume) / ceiling(659/NrGroups)))






data = data.frame(Group = 1:7,
                  Magnitude.CS = tapply(abs(Pred.CS), Group, mean),
                  Magnitude.CT = tapply(abs(Pred.CT), Group, mean))



############# BASE CASE ###########################


plotCS = ggplot(data, aes(x = Group, y = Magnitude.CS)) +
  geom_point(col = "red") +
  scale_x_continuous(breaks = 1:7, labels = 1:7) +
  xlab("Group") + ylab("Average Magnitude") +
  ggtitle("Cross-Sectional Regression")




plotCT = ggplot(data, aes(x = Group, y = Magnitude.CT)) +
  geom_point(col = "blue") +
  scale_x_continuous(breaks = 1:7, labels = 1:7) +
  xlab("Group") + ylab("Average Magnitude") +
  ggtitle("Cross-Temporal Regression")


plotVolume = grid.arrange(plotCS, plotCT)

ggsave(filename = "plotVolume.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = plotVolume,
       height = 5, width = 5)


