# Question 10 (in solving this question, I use the RF-based propensity scores)
## Calculating ATT and ATE weights
full_dt_rf_trimmed[, `:=`(
  att_weight = calc_weights_att(propensity_score_rf, treat),
  ate_weight = calc_weights_ate(propensity_score_rf, treat)
)]

## Calculating sums of weights, conditional on treatment
weight_sums <- full_dt_rf_trimmed[, .(
  att_sum = sum(att_weight),
  ate_sum = sum(ate_weight)
), by = treat]

full_dt_rf_trimmed <- full_dt_rf_trimmed[weight_sums, on = "treat"]

## Creating normalized weights and multiply by re78
full_dt_rf_trimmed[, `:=`(
  re78_weighted_att = (att_weight / att_sum) * re78,
  re78_weighted_ate = (ate_weight / ate_sum) * re78
)]

## Calculating ATT and ATE
att_mean_treated <- full_dt_rf_trimmed[treat == 1, mean(re78_weighted_att)]
att_mean_control <- full_dt_rf_trimmed[treat == 0, mean(re78_weighted_att)]
att_estimator_w <- att_mean_treated - att_mean_control

ate_mean_treated <- full_dt_rf_trimmed[treat == 1, mean(re78_weighted_ate)]
ate_mean_control <- full_dt_rf_trimmed[treat == 0, mean(re78_weighted_ate)]
ate_estimator_w <- ate_mean_treated - ate_mean_control

## Response: Yes, under the assumption of unconfoundedness, both the ATE and ATT estimators are unbiased. Typically, when ATE > ATT, it suggests that the treatment effect is larger for the broader population than for the treated group alone.
## In this case, however, the small difference between ATE and ATT indicates that the treatment effect is consistent across both the treated group and the broader population, implying a relatively homogeneous treatment effect.