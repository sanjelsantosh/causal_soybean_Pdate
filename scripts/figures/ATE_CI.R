# Make ATE , CI figre
# Create a data frame with your data
df <- data.frame(
  Method = c("Linear reg.\nmin. adjustment set", "Propensity score\nMatching"),
  ATE = c(3.3, 3.3),
  lowerCI = c(2.3, 1.32),
  upperCI = c(4.3, 5.35)
)


# Plot the means with whiskers
ggplot(df, aes(x = ATE, y = Method)) +
  geom_point(color = "deeppink3", size = 4) + #, shape = 18) +  # Red points for means
  geom_errorbarh(aes(xmin = lowerCI, xmax = upperCI), height = 0.2, color = "blue", position = position_dodge(0.4)) +  # Blue whiskers
  geom_text(aes(label = ATE),  vjust = -1) +
  labs(title = "Average Treatment Effect (ATE) with Confidence Intervals (0.95)",
       x = "Average Treatment Effect",
       y = "Methods") +
  theme_bw()
