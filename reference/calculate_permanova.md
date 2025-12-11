# Multivariate analysis - PERMANOVA

Function to perform a permutational analysis of variance

## Usage

``` r
calculate_permanova(
  abun_table,
  metadata,
  vars,
  distance = "bray",
  assess = "model",
  use_interaction = FALSE,
  strata = NULL
)
```

## Arguments

- abun_table:

  Feature abundance table with features as rows and samples as columns

- vars:

  Independent variables

- distance:

  Dissimilarity distance to calculate. Available options are those from
  [`vegan::vegdist()`](https://vegandevs.github.io/vegan/reference/vegdist.html)

- use_interaction:

  Calculate interaction of selected variables

- strata:

  Variable to define sample blocks

## Value

Object returned depend on the selected method
