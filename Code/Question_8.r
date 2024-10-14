# Question 8
## Preparing the data for Plotting
plot_data <- melt(
  full_dt[, .(treat, propensity_score, propensity_score_rf)],
  id.vars = "treat", 
  variable.name = "Method", 
  value.name = "Propensity_Score"
)

# Creating  the Plot
ggplot(plot_data, aes(x = Propensity_Score, color = Method, linetype = factor(treat))) +
  geom_density(size = 1.2) +
  scale_color_manual(values = c("red", "blue"), labels = c("Probit", "Random Forest")) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Control (D=0)", "Treatment (D=1)")) +
  labs(
    title = "Density of Propensity Scores by Method and Treatment Status",
    x = "Propensity Score",
    y = "Density",
    color = "Method",
    linetype = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")