# Question 7
## Fitting the random forest model
rf_formula <- as.factor(treat) ~ age + age_sq + education + black + hispanic + married + nodegree + re75

rf_model <- randomForest(
  formula = rf_formula,
  data = full_dt,
  ntree = 1000,
  mtry = 3,
  importance = TRUE
)

## Calculating the propensity score
full_dt[, propensity_score_rf := predict(rf_model, type = "prob")[, 2]]