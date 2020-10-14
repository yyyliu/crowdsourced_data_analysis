# here are the code for computing model fit and uncertainty
# it works for lm only

# --- (O)
# cross validation
fit <- cross_validation(df, model, '{{DV}}')
nrmse = fit / (max(df${{DV}}) - min(df${{DV}}))

# wrangle results
result <- tidy(model, conf.int = TRUE) %>%
  filter(term == '{{IV}}') %>%
  add_column(NRMSE = nrmse)

# get predictions
disagg_fit <- pointwise_predict(model, df) %>%
  select(
    observed = '{{DV}}',
    expected = fit
  ) %>%
  # remove rows where model can't make a prediction
  drop_na(expected)

# get uncertainty
uncertainty <- sampling_distribution(model, '{{IV}}') %>%
  select(estimate = coef)

write_csv(disagg_fit, '../results/disagg_fit_{{_n}}.csv')
write_csv(uncertainty, '../results/uncertainty_{{_n}}.csv')