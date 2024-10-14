# Question 3
## Generating 2000 random KS statistics
random_ks_statistics <- replicate(n_iterations, {
  random_assignment_ks <- sample(1:n_total, n_treated)
  re78_treated_random <- nsw[random_assignment_ks, re78]
  re78_control_random <- nsw[-random_assignment_ks, re78]
  ks_stat <- ks.test(re78_treated_random, re78_control_random)$statistic
  as.numeric(ks_stat)
})

## Obtaining the KS statistic for the original assignment
re78_treated_original <- nsw[treat == 1, re78]
re78_control_original <- nsw[treat == 0, re78]
original_ks_statistic <- ks.test(re78_treated_original, re78_control_original)$statistic

## Obtaining the p-value
sorted_ks_statistics_ks <- sort(random_ks_statistics)
p_value_ks <- mean(sorted_ks_statistics >= original_ks_statistic)