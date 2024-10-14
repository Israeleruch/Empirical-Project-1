# Question 13
## Creating a summary table comparing the ATE estimators
ate_comparison_table <- data.table(
  Estimator = c(
    "Simple Mean Difference",
    "Normalized Propensity Score Weighting",
    "Doubly Robust Estimator",
    "Cross-Fitted DR Estimator"
  ),
  ATE_Estimate = c(
    ate_estimator,
    ate_estimator_w,
    dr_ate_estimator,
    average_dr_ate_estimator
  )
)