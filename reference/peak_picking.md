# Apply Peak Picking

Functions to perform peak picking on the data:

- `apply_peak_picking`

  - Wrapper to perform peak picking with any of the methods

- `apply_peak_picking.cw`

  - Peak picking with the CentWave method

- `apply_peak_picking.mf`

  - Peak picking with the Massifquant method

- `apply_peak_picking.mq`

  - Peak picking with the MatchedFilter method

## Usage

``` r
apply_peak_picking(data, method = c("cw", "mf", "mq"), cores = 1, ...)

apply_peak_picking.cw(
  data,
  BPPARAM,
  ppm = 25,
  p_width = c(20, 50),
  snt = 3,
  noise = 1e+06,
  prefilter = c(1, 100),
  mz_diff = 0.001,
  ...
)

apply_peak_picking.mq(
  data,
  BPPARAM,
  ppm = 25,
  p_width = c(20, 50),
  snt = 3,
  noise = 1e+06,
  prefilter = c(1, 100),
  mz_diff = 0.001,
  ...
)

apply_peak_picking.mf(
  data,
  BPPARAM,
  bin = 0.1,
  fwhm = 30,
  sigma = 12.72,
  max = 10,
  steps = 2,
  ...
)
```

## Arguments

- data:

  An MSnExp object in *centroid* mode.

- method:

  Character. Specifies the peak picking method. Options include 'cw' for
  CentWave, 'mq' for Massifquant, and 'mf' for MatchedFilter.

- BPPARAM:

  BiocParallelParam-class object. Specifies the parallel processing
  parameters. Default is SnowParam with 4 workers.

- ppm:

  Numeric. Specifies the parts per million for m/z tolerance. Additional
  parameters include p_width, snt, noise, prefilter, mz_diff, bin, fwhm,
  sigma, max, and steps, each with specific roles in peak picking.

- p_width:

  Minium and maximum allowed peak width.

- snt:

  Minimum signal-to-noise threshold allowed.

- noise:

  Noise threshold.

- prefilter:

  Prefilter step cutoff (`c(k, I)`). Mass will be retained if they
  contain at least `k` peaks with intensity \>= `I`.

## Value

MSnExp-class object after applying peak picking.

## Details

Apply Peak Picking Function to apply peak picking parameters on a data
subset
