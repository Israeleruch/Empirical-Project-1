# Question 6
## Merging the two datasets
control_obs[, re74 := NULL]
full_dt <- rbind(nsw, control_obs, use.names = TRUE, fill = TRUE)

## Running a probit regression
full_dt[, age_sq := age^2]

probit_formula <- treat ~ age + age_sq + education + black + hispanic + married + nodegree + re75
probit_model <- glm(probit_formula, data = full_dt, family = binomial(link = "probit"))

## Calculating the propensity score
full_dt[, propensity_score := predict(probit_model, type = "response")]