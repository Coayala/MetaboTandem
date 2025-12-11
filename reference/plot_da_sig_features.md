# Heatmap significant features

Function to check the performance of the models that were fitted.

## Usage

``` r
plot_da_sig_features(
  abundance_table,
  metadata,
  diff_table,
  pval_thres,
  log2fc_thres,
  color_by,
  use_adjusted_pval = FALSE,
  cluster_feat = FALSE,
  cluster_samp = FALSE,
  color_vector = NULL
)
```

## Value

The return value, if any, from executing the function.
