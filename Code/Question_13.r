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

## Response: The Doubly Robust (DR) estimator is unbiased if either the propensity score model or the outcome models are correctly specified. 
## The cross-fitted DR estimator inherits this property while reducing the statistical dependence between the nuisance function, thereby achieving √N-asymptotic normality.
## Hence, the Cross-Fitted DR Estimator is the preferred estimator.