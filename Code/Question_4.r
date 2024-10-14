# Question 4
## Calculaing the standard deviation of the test statistics
std_dev <- sd(sorted_test_statistics)

## Constructing the 95% confidence interval
lower_bound_sharp <- ate_estimator - critical_value * std_dev
upper_bound_sharp <- ate_estimator + critical_value * std_dev
confidence_interval_sharp <- c(lower_bound_sharp, upper_bound_sharp)

## Displaying the confidence intervals side by side
interval_comparison <- data.table(
  Method = c("Standard CI", "Sharp Null CI"),
  Lower_Bound = c(confidence_interval[1], confidence_interval_sharp[1]),
  Upper_Bound = c(confidence_interval[2], confidence_interval_sharp[2])
)

interval_comparison