# Apply Peak Refinement

Function to apply peak picking parameters on a data subset.

## Usage

``` r
apply_peak_refinement(
  data,
  expand_rt = 2,
  expand_mz = 0,
  ppm = 10,
  min_prop = 0.75
)
```

## Arguments

- data:

  An MSnExp object in *centroid* mode.

- expand_rt:

  Numeric. Specifies the range to expand retention time during peak
  merging. Default is 2.

- expand_mz:

  Numeric. Specifies the range to expand m/z during peak merging.
  Default is 0.

- ppm:

  Numeric. Parts per million tolerance for peak merging.

- min_prop:

  Numeric. Minimum proportion of overlapping intensity required to merge
  peaks. Default is 0.75.

## Value

MSnExp-class object after refining peaks by merging neighboring peaks.

## Details

Apply Peak Refinement Function to apply peak picking parameters on a
data subset
