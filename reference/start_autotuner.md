# Start autotuner

This function to create Autotuner object and get peak signals

## Usage

``` r
start_autotuner(metadata, group, lag, threshold, influence, plot = FALSE)
```

## Arguments

- metadata:

  Sample information data.frame.

- group:

  Grouping variable.

- lag:

  Chromatographic scan points to test if next point is significant

- threshold:

  How many times larger intensity need to be to be considered
  significant

- influence:

  Factor to scale the magnitude of a significant scan

- plot:

  Logical value of whether to create a plot of the signals
