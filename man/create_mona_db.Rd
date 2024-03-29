% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_create_db.R
\name{create_mona_db}
\alias{create_mona_db}
\title{Create MoNA annotation database}
\usage{
create_mona_db(
  file,
  only_ms2 = TRUE,
  version = "0.0.1",
  source = "MassBank of America",
  link = "https://mona.fiehnlab.ucdavis.edu/",
  created_by = "MoNA",
  email = "email",
  rt = FALSE,
  threads = parallelly::availableCores()
)
}
\arguments{
\item{file}{Database file in mgf format}

\item{only_ms2}{Keep only metabolites that have MS2 data}

\item{version}{Database version}

\item{link}{Link to the database}

\item{created_by}{Database creater}

\item{email}{Email of database creater}

\item{rt}{If \code{TRUE} database contains information of retention time}

\item{threads}{Number of parallel processes for reading database}
}
\description{
This function is intended to create a metID database using data from MassBank of America.
This function was modified from the \code{construct_massbank_database()} function from
package \code{metID}
}
