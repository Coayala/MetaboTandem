# Normalize data

Function to apply different normalization methods to the data

## Usage

``` r
apply_normalization(abun_table, norm_method, log_transform = FALSE)

global_norm(abun_table, transform_data = TRUE)

median_norm(abun_table, transform_data = TRUE)

mean_norm(abun_table, transform_data = TRUE)

vsn_norm(abun_table)

cycloess_norm(abun_table)

max_norm(abun_table, transform_data = TRUE)
```

## Arguments

- abun_table:

  Matrix or data frame with the unnormalized peak intensities with peaks
  as rows and samples as columns

- norm_method:

  Normalization method to apply

- transform_data:

  Logical value of whether log transform data or not
