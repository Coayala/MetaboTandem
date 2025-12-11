# Multivariate analysis - Ordination

Function to calculate ordination with different methods

## Usage

``` r
calculate_ordination(abun_table, method, distance = "bray", group = NULL)

nmds_ordination(abun_table, distance)

pca_ordination(abun_table)

pcoa_ordination(abun_table, distance)
```

## Arguments

- abun_table:

  Feature abundance table

- method:

  Ordination method

- distance:

  Dissimilarity distance to calculate. Available options are those from
  [`vegan::vegdist()`](https://vegandevs.github.io/vegan/reference/vegdist.html)

## Value

Object returned depend on the selected method
