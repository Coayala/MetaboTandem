# Filter by prevalence

Function to filter features based on prevalence

Function to filter features with low variances

## Usage

``` r
apply_prevalence_filtering(abun_table, min_perc_samples)

apply_variance_filtering(abun_table, filter_method, perc_remove)

iqr_filtering(abun_table, perc_remove)

sd_filtering(abun_table, perc_remove)

mad_filtering(abun_table, perc_remove)

rsd_filtering(abun_table, perc_remove)
```

## Arguments

- abun_table:

  abun_table or data frame with the non-normalized peak intensities with
  peaks as rows and samples as columns

- min_perc_samples:

  Minimum percentage of samples a feature need to be present

- filter_method:

  Filter method to apply

- perc_remove:

  Percentage of features to remove from the analysis
