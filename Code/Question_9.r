# Question 9
## Trimming datasets based on propensity scores
full_dt_probit_trimmed <- full_dt[
  propensity_score %between% c(0.1, 0.9)
]
full_dt_rf_trimmed <- full_dt[
  propensity_score_rf %between% c(0.1, 0.9)
]

## Creating functions to calculate IPW weights for ATT and ATE
calc_weights_att <- function(p_score, treat) {
  treat + (1 - treat) * (p_score / (1 - p_score))
}

calc_weights_ate <- function(p_score, treat) {
  treat / p_score + (1 - treat) / (1 - p_score)
}

## Calculating the weighted outcomes
full_dt_probit_trimmed[, `:=`(
  re78_weighted_att = re78 * calc_weights_att(propensity_score, treat),
  re78_weighted_ate = re78 * calc_weights_ate(propensity_score, treat)
)]

full_dt_rf_trimmed[, `:=`(
  re78_weighted_att = re78 * calc_weights_att(propensity_score_rf, treat),
  re78_weighted_ate = re78 * calc_weights_ate(propensity_score_rf, treat)
)]

## Defining an empty table to store the results
weighted_balance_table <- data.table(
  Dataset = character(), 
  Mean_Diff_p_value = numeric(), 
  SMD = numeric(), 
  KS_p_value = numeric()
)

## Creating a list of datasets and their weighted re78 columns
datasets <- list(
  list(data = full_dt_probit_trimmed, column = "re78_weighted_att", name = "Probit_ATT"),
  list(data = full_dt_probit_trimmed, column = "re78_weighted_ate", name = "Probit_ATE"),
  list(data = full_dt_rf_trimmed, column = "re78_weighted_att", name = "RF_ATT"),
  list(data = full_dt_rf_trimmed, column = "re78_weighted_ate", name = "RF_ATE")
)

## Looping through datasets and performing tests for each column (ATT and ATE)
for (dataset in datasets) {
  data <- dataset$data
  column <- dataset$column
  dataset_name <- dataset$name
  
  treated_data <- data[treat == 1, get(column)]
  control_data <- data[treat == 0, get(column)]

  ## Calculating means, variances, and sample sizes
  mean_treated <- mean(treated_data)
  mean_control <- mean(control_data)
  var_treated <- var(treated_data)
  var_control <- var(control_data)
  n_treated <- length(treated_data)
  n_control <- length(control_data)
  
  ## Mean Difference
  t_result <- t.test(treated_data, control_data)
  mean_diff_p_value <- t_result$p.value
  
  ## Standardized Mean Difference (SMD)
  SMD_result <- calc_SMD(mean_treated, mean_control, var_treated, var_control, n_treated, n_control)
  
  ## Kolmogorov-Smirnov Test
  KS_result <- ks.test(treated_data, control_data)
  ks_p_value <- KS_result$p.value
  
  ## Storing the results in the table
  weighted_balance_table <- rbind(
    weighted_balance_table, 
    data.table(
      Dataset = dataset_name,
      Mean_Diff_p_value = mean_diff_p_value,
      SMD = SMD_result,
      KS_p_value = ks_p_value
    )
  )
}

## Response: Both Probit and Random Forest models perform well, with substantial treatment effects observed (SMD > 1) and highly significant p-values across both ATT and ATE estimators. 
## The similarity in results is reassuring, suggesting that both methods and estimators effectively capture the treatment effects. 
## While the Random Forest model offers greater flexibility in capturing non-linear relationships and interactions, the Probit model remains valuable for its simplicity and interpretability.