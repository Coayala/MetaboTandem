% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Module_loadData.R
\name{spectraServer}
\alias{spectraServer}
\title{Spectra uploading server-side processing}
\usage{
spectraServer(id, metadata)
}
\arguments{
\item{id}{character used to specify namespace, see \code{\link[shiny:NS]{shiny::NS}}}

\item{metadata}{dataframe with sample information}
}
\value{
A centroided \linkS4class{MSnExp} object
}
\description{
Spectra uploading server-side processing
}
