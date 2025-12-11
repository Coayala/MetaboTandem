# Test Peak Picking

Tests peak picking parameters on a subset of the data.

## Usage

``` r
test_peak_picking(
  data,
  mz_range,
  rt_range,
  p_width,
  snt,
  noise,
  prefilter = c(1, 100),
  cores = 1
)
```

## Arguments

- data:

  An MSnExp object in *centroid* mode.

- snt:

  Minimum signal-to-noise threshold allowed

- noise:

  Noise threshold

- prefilter:

  Prefilter step cutoff (`c(k, I)`). Mass will be retained if they
  contain at least `k` peaks with intensity \>= `I`

- ...:

  Parameters to pass to the selected method

- mz.range:

  *mz* range (`numeric(2)`) to test peak picking parameters.

- rt.range:

  *rt* range (`numeric(2)`) to test peak picking parameters.

- p.width:

  Minium and maximum allowed peak width

## Value

Modified MSnExp-class object with tested peak picking parameters.

## Details

Test peak picking Function to test peak picking parameters on a data
subset
