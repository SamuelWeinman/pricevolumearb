t <- 4348
d <- 20


standardisedVolume <- unlist(Volume[, t - 1] / apply(Volume[(t - d):(t - 1)], 1, mean))

data <- data.frame(standardisedVolume = standardisedVolume)

standardisedVolume.Distr <- ggplot(data = data, aes(standardisedVolume)) +
  geom_histogram(bins = 30, fill = "red", col = "black") +
  xlab("Standardised volume") +
  ylab("Frequency") +
  scale_x_continuous(
    breaks = seq(-0.5, 5, by = 0.5),
    limits = c(0, 4.6)
  )



ggsave(
  filename = "standardisedVolume.Distr.png",
  path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
  plot = standardisedVolume.Distr,
  height = 4, width = 5
)
