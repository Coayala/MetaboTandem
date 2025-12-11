# Get annotation tables

Function to annotate features using MS1 and MS2 information

## Usage

``` r
get_annotation_tables(
  feature_table,
  feature_spectra,
  selected_dbs = c("massbank", "mona", "hmdb_exp", "hmdb_pred", "gnps"),
  adducts = c("[M+H]+"),
  tolerance = 0.005,
  ppm = 5,
  req_precursor = TRUE,
  distance_thres = 0.5,
  candidates = 1
)
```

## Arguments

- feature_table:

  Table with features mz and rt values

- feature_spectra:

  Features spectra

- selected_dbs:

  Vector with names of the databases to be used

- adducts:

  Vector with the adducts to be used for MS1 annotation

- tolerance:

  Allowed differences in m/z

- ppm:

  Allowed differences in ppm

- req_precursor:

  Check precursor for MS2 annotation

- distance_thres:

  Distance threshold for annotation

- candidates:

  How many annotations to keep for each feature

## Value

List with annotations from each of the databases
