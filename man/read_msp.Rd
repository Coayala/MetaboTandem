% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_read_ms2.R
\name{read_msp}
\alias{read_msp}
\title{Read msp data}
\usage{
read_msp(file, threads = parallelly::availableCores())
}
\arguments{
\item{file}{Database file in msp format}
}
\description{
This function is intended to read data in msp format.
This function was modified from the \code{read_msp_mona()} function from
package \code{metID}
}
