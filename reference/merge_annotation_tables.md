# Merge annotation tables

Function to merge annotations from multiple databases

## Usage

``` r
merge_annotation_tables(annotation_tables_list, feature_table, candidates = 1)
```

## Arguments

- annotation_tables_list:

  List with annotations to merge

- candidates:

  How many annotations to keep for each feature

## Value

Dataframe with top annotation from all databases
