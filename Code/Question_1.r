# Question 1
## Obtaining group size, mean, and variance of treated and control groups
n_treated <- nsw[treat == 1, .N]
n_control <- nsw[treat == 0, .N]

mean_treated <- nsw[treat == 1, mean(re78)]
mean_control <- nsw[treat == 0, mean(re78)]

var_treated <- nsw[treat == 1, var(re78)]
var_control <- nsw[treat == 0, var(re78)]

## Calculating the ATE estimator and its standard error
ate_estimator <- mean_treated - mean_control
se_ate <- sqrt(var_treated / n_treated + var_control / n_control)

## Calculating the 95% confidence interval
critical_value <- qnorm(0.975)
lower_bound <- ate_estimator - critical_value * se_ate
upper_bound <- ate_estimator + critical_value * se_ate
confidence_interval <- c(lower_bound, upper_bound)