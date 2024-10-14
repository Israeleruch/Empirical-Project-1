# Question 12
## Partitioning the data into 5 folds
num_folds <- 5
full_dt[, fold := sample(rep(1:num_folds, length.out = .N))]

ate_estimates <- numeric(num_folds)

## Iterating through the 5 folds
for (k in 1:num_folds) {
  train_data <- full_dt[fold != k]
  test_data <- full_dt[fold == k]

  ## Fitting Random Forest models for μ1 and μ0
  rf_formula_expect_cf <- re78 ~ age + age_sq + education + black + hispanic + married + nodegree + re75

  rf_treated <- randomForest(
    formula = rf_formula_expect_cf,
    data = train_data[treat == 1],
    ntree = 1000,
    mtry = 3,
    importance = TRUE
  )

  rf_control <- randomForest(
    formula = rf_formula_expect_cf,
    data = train_data[treat == 0],
    ntree = 1000,
    mtry = 3,
    importance = TRUE
  )

  ## Fitting a Random Forest model for propensity scores
  rf_propensity <- randomForest(
    formula = treat ~ age + age_sq + education + black + hispanic + married + nodegree + re75,
    data = train_data,
    ntree = 1000,
    mtry = 3,
    importance = TRUE
  )

  ## Predicting μ1(xi), μ0(xi), and propensity scores for the left-out fold
  test_data[, mu1 := predict(rf_treated, newdata = .SD)]
  test_data[, mu0 := predict(rf_control, newdata = .SD)]
  test_data[, propensity_score := predict(rf_propensity, newdata = .SD, type = "response")]

  ## Trimming the observations in the left-out fold based on propensity scores
  trimmed_test_data <- test_data[propensity_score %between% c(0.1, 0.9)]

  ## Computing and storing the DR ATE estimator 
  trimmed_test_data[, dr_ate := (mu1 - mu0) +
    (treat * (re78 - mu1) / propensity_score) -
    ((1 - treat) * (re78 - mu0) / (1 - propensity_score))]

  ate_estimates[k] <- mean(trimmed_test_data$dr_ate)
}

## Computing the average ATE estimate across all folds
average_dr_ate_estimator <- mean(ate_estimates)

## Response: By partitioning the data into several folds, cross-fitting reduces the statistical dependence between the nuisance functions (such as propensity scores and outcome models) and the ATE estimator. 
## If the machine learning models satisfy the product rate condition, the estimator achieves √N-asymptotic normality.
