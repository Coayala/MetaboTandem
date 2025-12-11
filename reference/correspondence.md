# Apply Correspondence

Function to apply peak alignment

## Usage

``` r
apply_correspondence(data, metadata, method = c("pd", "np"), ...)

apply_correspondence.pd(
  data,
  metadata,
  group_by,
  bandwidth = 30,
  bin_size = 0.25,
  ppm = 0,
  min_fraction = 0.9,
  max_features = 30,
  ...
)

apply_correspondence.np(
  data,
  metadata,
  group_by,
  mzvsrtbal = 10,
  abs_mz = 0.2,
  abs_rt = 15,
  knn = 10,
  ...
)
```

## Arguments

- data:

  An MSnExp object in *centroid* mode.

- metadata:

  Sample information data.frame.

## Value

MSnExp-class object after alignment.
