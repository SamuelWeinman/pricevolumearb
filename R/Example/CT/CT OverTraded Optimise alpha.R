Alpha.Candidates <- seq(40, 250, by = 15)

Scores <- list()

for (i in 1:length(Alpha.Candidates)) {
  print(i)
  A <- Alpha.Candidates[i]
  Preds <- crossTemporalRegressionOvertrade(Volume, Returns,
    start = 500, end = ncol(Returns),
    H = 252, nr_pc.V = 25, nr_pc = 20,
    alpha = A,
    L = 45, b_sensitivity = 0.01
  )
  s <- performFullAnalysis(Returns, Preds, r = 30, Q = (1:4) / 4)
  Scores <- append(Scores, list(S))
  Sys.time()
}
names(Scores) <- Alpha.Candidates

write.csv(x = Scores, file = "./Results/Predictions/CT OverTrading/Scores.CT.OverTrading.OverAlpha.csv")




Mean <- sapply(1:length(Scores), function(i) {
  s <- Scores[[i]]
  mean(S$Regular$PnL)
})

sd <- sapply(1:length(Scores), function(i) {
  s <- Scores[[i]]
  sd(S$Regular$PnL)
})



data <- data.frame(
  alpha = Alpha.Candidates,
  sd = sd,
  Mean = Mean
)


Plot.Mean <- ggplot(data, aes(x = alpha, y = Mean)) +
  geom_point(color = "#543005") +
  ylab("Mean PnL")


Plot.Sd <- ggplot(data, aes(x = alpha, y = sd)) +
  geom_point(color = "#543005") +
  ylab("Standard Deviation PnL")


Plot.Sharpe <- ggplot(data, aes(x = alpha, y = Mean / sd * sqrt(252))) +
  geom_point(color = "#543005") +
  ylab("Sharpe Ratio")



Layout <- matrix(c(1, 3, 2, 3), nrow = 2)
Plot <- grid.arrange(Plot.Mean, Plot.Sd, Plot.Sharpe, layout_matrix = Layout)


ggsave(
  filename = "Performance.OverTrading.CT.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = Plot
)




Scores <- read.csv("./Results/Predictions/CT OverTrading/Scores.CT.OverTrading.OverAlpha.csv")


Sharpe <- list()
for (i in 1:15) {
  Sharpe[[2 * i - 1]] <- unique(Scores[, which(str_sub(colnames(Scores), -6, -1) == "Sharpe")[2 * i - 1]])
  Sharpe[[2 * i]] <- unique(Scores[, which(str_sub(colnames(Scores), -6, -1) == "Sharpe")[2 * i]])
}

PPT <- list()
for (i in 1:15) {
  PPT[[2 * i - 1]] <- unique(Scores[, which(str_sub(colnames(Scores), -3, -1) == "PPT")[2 * i - 1]])
  PPT[[2 * i]] <- unique(Scores[, which(str_sub(colnames(Scores), -3, -1) == "PPT")[2 * i]])
}


Scores <- list()
for (i in 1:15) {
  RegularList <- list(PPT = PPT[[2 * i - 1]], Sharpe = Sharpe[[2 * i - 1]])
  StandardList <- list(PPT = PPT[[2 * i]], Sharpe = Sharpe[[2 * i]])

  Score <- list(Regular = RegularList, Standard = StandardList)

  Scores <- append(Scores, list(Score))
}


SharpePPT.OT.CT.OverAlpha <- createSharpePPTPlot(
  Scores = Scores[seq(1, 15, by = 2)],
  Labels = Alpha.Candidates[seq(1, 15, by = 2)],
  BaseModels = 1,
  Type = "CT"
)


ggsave(
  filename = "SharpePPT.OT.CT.OverAlpha.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = SharpePPT.OT.CT.OverAlpha,
  width = 5, height = 5
)
