# Multivariate analysis - Clustering

Function to calculate ordination with different methods

## Usage

``` r
calculate_clustering(
  abun_table,
  metadata,
  color_by,
  distance = "euclidean",
  cluster_algorithm = "ward.D2",
  add_kmeans = FALSE,
  k = 3,
  plot = TRUE,
  color_vector = NULL
)
```

## Arguments

- abun_table:

  Feature abundance table

- k:

  Selecte K (for kmeans method)

- method:

  Clustering method
