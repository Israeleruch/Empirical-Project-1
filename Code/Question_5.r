# Question 5
## Creating a list of covariates
covariates <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75")

## Creating an empty data.table to store results
balance_table <- data.table(
  Covariate = covariates,
  Mean_Diff_p_value = NA_real_,
  SMD = NA_real_,
  KS_p_value = NA_real_
)

# Defining a function to calculate the absolute standardized mean difference
calc_SMD <- function(mean_treated, mean_control, var_treated, var_control, n_treated, n_control) {
  pooled_sd <- sqrt((var_treated*(n_treated - 1) + var_control*(n_control - 1)) / (n_treated + n_control - 2))
  abs(mean_treated - mean_control) / pooled_sd
}

## Performing tests for each covariate
for (i in 1:length(covariates)) {
  covariate <- covariates[i]

  treated_data <- nsw[treat == 1, get(covariate)]
  control_data <- nsw[treat == 0, get(covariate)]
  
  mean_treated <- mean(treated_data)
  mean_control <- mean(control_data)
  var_treated <- var(treated_data)
  var_control <- var(control_data)
  n_treated <- length(treated_data)
  n_control <- length(control_data)
  
  ## Mean difference
  t_result <- t.test(treated_data, control_data)
  balance_table[i, Mean_Diff_p_value := t_result$p.value]
  
  ## Absolute standardized mean difference
  SMD_result <- calc_SMD(mean_treated, mean_control, var_treated, var_control, n_treated, n_control)
  balance_table[i, SMD := SMD_result]
  
  ## Kolmogorov-Smirnov test
  KS_result <- ks.test(treated_data, control_data)
  balance_table[i, KS_p_value := KS_result$p.value]
}

## Testing the joint null hypothesis of mean differences using Hotelling's T-squared test
treated_matrix <- as.matrix(nsw[treat == 1, ..covariates])
control_matrix <- as.matrix(nsw[treat == 0, ..covariates])

hotelling_result <- hotelling.test(treated_matrix, control_matrix)

## Response: Based on the results, the randomization assumption is largely reasonable, as most covariates appear well-balanced between the treated and control groups. 
## However, there is some evidence of imbalance in the 'nodegree' and 'education' covariates, which may warrant further adjustment in the analysis to account for these differences.