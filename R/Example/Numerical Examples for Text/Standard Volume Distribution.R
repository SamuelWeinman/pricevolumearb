t <- 4348
d <- 20


standardised_volume <- unlist(Volume[, t - 1] / apply(Volume[(t - d):(t - 1)], 1, mean))

data <- data.frame(standardised_volume = standardised_volume)

standardised_volume.Distr <- ggplot(data = data, aes(standardised_volume)) +
  geom_histogram(bins = 30, fill = "red", col = "black") +
  xlab("Standardised volume") +
  ylab("Frequency") +
  scale_x_continuous(
    breaks = seq(-0.5, 5, by = 0.5),
    limits = c(0, 4.6)
  )



ggsave(
  filename = "standardised_volume.Distr.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = standardised_volume.Distr,
  height = 4, width = 5
)
