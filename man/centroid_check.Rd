% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_load_data.R
\name{centroid_check}
\alias{centroid_check}
\title{Check Centroided}
\usage{
centroid_check(data, transform = TRUE)
}
\arguments{
\item{data}{MSnExp-class object. Represents mass spectrometry data.}

\item{transform}{Logical. If TRUE, transforms the data into centroid mode using available transformation functions. Default is TRUE.}
}
\value{
MSnExp-class object with data either confirmed as centroid or transformed into centroid mode.
}
\description{
Checks if the data is in centroid mode.
}
\details{
Check centroided
This function checks if data is centroided
}
