# Question 2
## Setup
n_iterations <- 2000
n_total <- nsw[,.N]

## Generating 2000 random test statistics
random_test_statistics <- replicate(n_iterations, {
  random_assignment <- sample(1:n_total, n_treated)
  mean_treated_random <- nsw[random_assignment, mean(re78)]
  mean_control_random <- nsw[-random_assignment, mean(re78)]
  mean_treated_random - mean_control_random
})

## Obtaining the p-value
sorted_test_statistics <- sort(random_test_statistics)
p_value_sharp <- mean(abs(sorted_test_statistics) >= ate_estimator)