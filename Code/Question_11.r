# Question 11
## Fitting the random forest model
rf_formula_expect <- re78 ~ age + age_sq + education + black + hispanic + married + nodegree + re75

rf_treated <- randomForest(
  formula = rf_formula_expect,
  data = full_dt_rf_trimmed[treat == 1],
  ntree = 1000,
  mtry = 3,
  importance = TRUE
)

rf_control <- randomForest(
  formula = rf_formula_expect,
  data = full_dt_rf_trimmed[treat == 0],
  ntree = 1000,
  mtry = 3,
  importance = TRUE
)

## Calculating conditional expectations
full_dt_rf_trimmed[, mu1 := predict(rf_treated, newdata = .SD)]
full_dt_rf_trimmed[, mu0 := predict(rf_control, newdata = .SD)]

## Calculating the Doubly Robust estimator for ATE
full_dt_rf_trimmed[, dr_ate := (mu1 - mu0) +
  (treat * (re78 - mu1) / propensity_score_rf) -
  ((1 - treat) * (re78 - mu0) / (1 - propensity_score_rf))]

dr_ate_estimator <- mean(full_dt_rf_trimmed$dr_ate)

## Response: Unlike the IPW estimator, the DR estimator ensures that if either the propensity score model or the outcome models are correctly specified, the estimator remains unbiased. 
## This flexibility allows for the use of nonparametric methods like Random Forest, as performed here. 
## To minimize bias, it is important that the machine learning models avoid overfitting, ensuring they generalize well to new data. 
## Additionally, for the estimator to achieve √N-asymptotic normality, the models must satisfy the product rate condition, which requires both the propensity score and outcome models to converge at appropriate rates.