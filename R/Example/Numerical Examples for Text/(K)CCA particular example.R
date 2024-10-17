library(latex2exp)



t <- 4096 + 252
H <- 100
HV <- 100
standardised_volume <- Volume / t(roll_mean(t(as.matrix(Volume)), width = d))

# ORIGINAL VARIABLES
X <- Returns[, (t - H):(t - 1)]
Y <- standardised_volume[, (t - HV):(t - 1)]

dataXY <- data.frame(X = X[, 1], Y = Y[, 1])

plotXY <- ggplot(dataXY, aes(x = X, y = Y)) +
  geom_point(col = "#F8766D") +
  ggtitle("Original Data") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab(TeX("$x^{(1)}$")) +
  ylab(TeX("$y^{(1)}$"))


# CCA VARIABLES
C <- cc(X, Y)
X.C <- as.matrix(X) %*% as.matrix(C$xcoef)
Y.C <- as.matrix(Y) %*% as.matrix(C$ycoef)


dataXY.C <- data.frame(X = X.C[, 1], Y = Y.C[, 1])

plotXY.C <- ggplot(dataXY.C, aes(x = X, y = Y)) +
  geom_point(col = "#00BA38") +
  xlim(-3, 3) +
  ylim(-7.5, 0) +
  ggtitle("CCA Transformed Data") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab(TeX("$x^{(1)}$")) +
  ylab(TeX("$y^{(1)}$"))


# KCCA VARIABLES
k <- kcca(as.matrix(X), as.matrix(Y), ncomps = 100)
X.K <- k@xcoef
Y.K <- k@ycoef

dataXY.K <- data.frame(X = X.K[, 1], Y = Y.K[, 1])

plotXY.K <- ggplot(dataXY.K, aes(x = X, y = Y)) +
  geom_point(col = "#619CFF") +
  ggtitle("KCCA Transformed Data") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab(TeX("$x^{(1)}$")) +
  ylab(TeX("$y^{(1)}$"))


plot <- grid.arrange(plotXY, plotXY.C, plotXY.K, nrow = 1)


ggsave(
  filename = "KCCA.TransformedData.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = plot,
  width = 8, height = 5
)
