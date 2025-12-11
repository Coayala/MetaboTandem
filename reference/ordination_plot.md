# Multivariate analysis - Plot Ordination

Function to plot ordination results

## Usage

``` r
plot_ordination(
  ord_object,
  metadata,
  group_by,
  add_sample_names = FALSE,
  add_variables = FALSE,
  add_ellipse = FALSE,
  color_vector = NULL,
  abundances = NULL,
  plot = TRUE
)

plot_ordination.nmds(
  ord_object,
  metadata,
  group_by,
  add_sample_names,
  add_variables,
  add_ellipse,
  color_vector,
  abundances,
  plot
)

plot_ordination.pca(
  ord_object,
  metadata,
  group_by,
  add_sample_names,
  add_variables,
  add_ellipse,
  color_vector,
  plot
)

plot_ordination.pcoa(
  ord_object,
  metadata,
  group_by,
  add_sample_names,
  add_variables,
  add_ellipse,
  color_vector,
  abundances,
  plot
)
```

## Arguments

- ord_onj:

  Feature abundance table

- method:

  Ordination method

- distance:

  Dissimilarity distance to calculate. Available options are those from
  [`vegan::vegdist()`](https://vegandevs.github.io/vegan/reference/vegdist.html)

## Value

Object returned depend on the selected method
