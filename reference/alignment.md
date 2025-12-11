# Apply Peak alignment

Function to apply peak alignment

## Usage

``` r
apply_alignment(data, metadata, method = c("pg", "ow", "lama"), ...)

apply_alignment.pg(
  data,
  metadata,
  group_by,
  bin_size_pg = 0.25,
  ppm_bin = 0,
  min_fraction = 0.9,
  extra_peaks = 1,
  smooth = c("loess", "linear"),
  span = 1,
  family = c("gaussian", "symmetric"),
  subset_samples = NULL,
  subset_adjust = c("previous", "average"),
  ...
)

apply_alignment.ow(
  data,
  metadata,
  bin_size_ow = 1,
  smooth_responsiveness = 1,
  distance_function = c("cor", "cor_opt", "cov", "prd", "euc"),
  local_alignment = FALSE,
  subset_samples = NULL,
  subset_adjust = c("previous", "average"),
  ...
)

apply_alignment.lama(
  data,
  metadata,
  lama_file,
  lama_method = c("loess", "gam"),
  span_lama = 0.5,
  ppm_lama = 20,
  tolerance_mz = 0,
  tolerance_rt = 5,
  gam_smoothing = c("tp", "ts", "ds", "cr", "cs", "cc", "sos", "bs", "ps", "re", "mrf",
    "gp", "so"),
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
