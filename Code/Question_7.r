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

## Response: By using Random Forest, we are relaxing the parametric assumption about the relationship between the covariates and the treatment assignment.
## The Random Forest model can capture non-linear relationships and complex interactions among covariates automatically.
## The main cost is in Random Forest models, that We can’t easily interpret how individual covariates affect the treatment assignment.
