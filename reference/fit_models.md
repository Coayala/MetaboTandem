# Fit linear models

Function to perform differential abundance analysis among the specified
groups

## Usage

``` r
fit_model(
  abun_table,
  metadata,
  model_type,
  vars = NULL,
  fix_vars = NULL,
  rand_vars = NULL
)

fit_model.lm(feature_table, vars)

fit_model.lme(feature_table, fix_vars, rand_vars)
```

## Value

The return value, if any, from executing the function.
