# Check Centroided

Checks if the data is in centroid mode.

## Usage

``` r
centroid_check(data, transform = TRUE)
```

## Arguments

- data:

  MSnExp-class object. Represents mass spectrometry data.

- transform:

  Logical. If TRUE, transforms the data into centroid mode using
  available transformation functions. Default is TRUE.

## Value

MSnExp-class object with data either confirmed as centroid or
transformed into centroid mode.

## Details

Check centroided This function checks if data is centroided
