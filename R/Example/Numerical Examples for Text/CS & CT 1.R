#################### SET PARAMETERS ##########################

t <- 4096 + 252
colnames(Returns)[t]

H <- 252
nr_pc <- 20
L <- 45

###################### PERFORM CALCULATIONS ##################

# LAST H DAYS RETURN
ReturnsShort <- Returns[, (t - H):(t - 1)]

# CONSTRUCT CORRELATION MATRIX
Rho <- constructRho(ReturnsShort)

# EXTRACT EIGEN
E <- eigen(Rho)

# CALCULATE THE SD OF THE RETURNS, BASED ON STOCK
SigmaPerStock <- apply(ReturnsShort, 1, sd) # the ith entry is the sd of the ith stock


# CONSTRUCT EIGENPORTFOLIOS
Q <- E$vectors / SigmaPerStock



###################### PLOT OF FIRST AND SECOND EIGENPORTFOLIO ##################

FirstPortfolio <- Q[, 1]
mean(FirstPortfolio < 0) # almost all negative


Order.1 <- order(abs(FirstPortfolio), decreasing = T)
Values.1 <- FirstPortfolio[Order.1][1:20] # 20 STRONGEST VALUES
Stocks.1 <- rownames(Returns)[Order.1][1:20] # CORRESPONDING NAMES
Stocks.1 <- factor(Stocks.1, levels = Stocks.1) # REORDER
data.1 <- data.frame(Values = Values.1, Stocks = as.factor(Stocks.1))

# CREATE FIRST PLOT
EigenPortfolioDistr.1 <- ggplot(data.1, aes(x = Stocks, y = Values)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#1B9E77") +
  xlab("Stock") +
  ylab("Amount invested ($)") +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))




SencondPortfolio <- Q[, 2]


Order.2 <- order(abs(SencondPortfolio), decreasing = T)
Values.2 <- SencondPortfolio[Order.2][1:20] # 20 STRONGEST VALUES
Stocks.2 <- rownames(Returns)[Order.2][1:20] # CORRESPONDING NAMES
Stocks.2 <- factor(Stocks.2, levels = Stocks.2) # REORDER
data.2 <- data.frame(Values = Values.2, Stocks = as.factor(Stocks.2))

# CREATE FIRST PLOT
EigenPortfolioDistr.2 <- ggplot(data.2, aes(x = Stocks, y = Values)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#D95F02") +
  xlab("Stock") +
  ylab("Amount invested ($)") +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))






###################### PLOT OF EIGENPORTFOLIO RETURNS ##################

# EIGENPORTOLIO RETURNS
F <- t(Q) %*% as.matrix(ReturnsShort)

# EXTRACT FIRST 20 EIGENPORTFOLIOS
EigenReturns <- F


CumReturn <- apply(EigenReturns, 1, cumsum)


data <- data.frame(
  Value = as.numeric(CumReturn[, 1:2]),
  EigenPortfolio = as.factor(as.numeric(sapply(1:2, function(i) rep(i, H)))),
  Dates = rep(as.Date(colnames(Returns)[(t - H):(t - 1)], format = "%Y%m%d"), 2)
)


plot.EigenReturn <- ggplot(data = data, aes(x = Dates, y = Value)) +
  ylab("Profit ($)") +
  xlab("Date") +
  geom_line(aes(colour = EigenPortfolio)) + # EigenPortfolio as colour
  scale_colour_brewer(palette = "Dark2") + # palette
  theme(legend.position = "top") +
  labs(colour = "Eigenportfolio")





layout <- matrix(c(1, 3, 2, 3), nrow = 2)
EigenExample.plot <- grid.arrange(EigenPortfolioDistr.1, EigenPortfolioDistr.2, plot.EigenReturn,
  layout_matrix = layout
)

ggsave(
  filename = "EigenExample.plot.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = EigenExample.plot,
  height = 5, width = 6
)



####### PLOT SPRECTRUM#####
data <- data.frame(
  PropExplain = E$values[1:50] / sum(E$values),
  Nr = 1:50
)

EigenPlot <- ggplot(data, aes(x = Nr, y = PropExplain)) +
  geom_bar(stat = "identity", position = "dodge", fill = "red") +
  ylab("Proportion variability explained (%)") +
  xlab("Ordered Component")



ggsave(
  filename = "EigenPlot.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = EigenPlot,
  height = 4, width = 5
)
