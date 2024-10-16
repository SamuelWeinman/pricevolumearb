i = which(row.names(Returns) == "BOH")


y = unlist(ReturnsShort[i, (H-L+1):H])
X = F[1:20, (H-L+1):H]

model = lm(y~t(X))

X = cumsum(model$residuals)

X.model = lm(X[2:L] ~ X[1:(L-1)])

Coeff = estimateCoefficeients(list(model), b_sensitivity = 0.01)
kappa = Coeff$K
m = Coeff$m
sigma2 = Coeff$Sigma.Squared
sigma2.eq = Coeff$sigma_eq_squared

Pred = m/sqrt(sigma2.eq)




################# TRUE RESIDUAL OBSERVED ###########################


true = Returns[i, t]
estimate = as.numeric(model$coefficients) %*% c(1, as.numeric( t(Q[, 1:20]) %*% as.matrix(Returns[, t]) ) )


residual = true-estimate

pvalue = 2* pnorm(residual - m, sd = sqrt(sigma2.eq), lower.tail = FALSE)

################# TOMORROWS RETURN DISTRIBUTION ############################






x = seq(m-3*sqrt(sigma2.eq), m+3*sqrt(sigma2.eq), length.out = 10^5)
y = dnorm(x-m, sd = sqrt(sigma2.eq))


data = data.frame(x = x, y = y, 
                  pointx = c(residual, rep(100, length(x)-1)),
                  pointy = c(dnorm(residual-m, sd = sqrt(sigma2.eq)), rep(100, length(x)-1)))


plot = ggplot(data, aes(x = x, y = y)) +
        geom_line(col = "red") +
        geom_point(x = data$pointx, y = data$pointy, col = "blue", size = 3) +
        ylab("Estimated density") + xlab("Residual on September 1, 2017")



ggsave(filename = "Residual distribution.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = plot,
       height = 5, width = 5)







