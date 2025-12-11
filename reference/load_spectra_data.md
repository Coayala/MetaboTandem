# Load Spectra Data

Loads spectra data in mzML or mzXML format using the xcms package.

## Usage

``` r
load_spectra_data(datadir, metadata, format = "mzML")
```

## Arguments

- datadir:

  Path to the directory containing the data files.

- metadata:

  Contains sample information. Each row represents a sample and columns
  contain metadata information.

- format:

  Specifies the format of the data files. Default is 'mzML'. Supported
  formats include 'mzML' and 'mzXML'.

- mode:

  Specifies the mode of operation. Default is 'onDisk'. Other modes
  depend on xcms package capabilities.

## Value

An object containing the loaded spectra data.

## Details

Load Spectra data This function is intended to load the spectra data in
mzML or mzXML format using the xcms package
