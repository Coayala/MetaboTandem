% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_annotation.R
\name{mod_metIdentify}
\alias{mod_metIdentify}
\title{Modified - Identify metabolites using}
\usage{
mod_metIdentify(
  ms1.data,
  ms1.info,
  ms2.data = NULL,
  ms2.info,
  ms1.ms2.match.mz.tol = 25,
  ms1.ms2.match.rt.tol = 10,
  ms1.match.ppm = 25,
  ms2.match.ppm = 30,
  mz.ppm.thr = 400,
  ms2.match.tol = 0.5,
  fraction.weight = 0.3,
  dp.forward.weight = 0.6,
  dp.reverse.weight = 0.1,
  rt.match.tol = 30,
  polarity = c("positive", "negative"),
  ce = "all",
  column = c("rp", "hilic"),
  ms1.match.weight = 0.25,
  rt.match.weight = 0.25,
  ms2.match.weight = 0.5,
  total.score.tol = 0.5,
  candidate.num = 3,
  database_name,
  database,
  threads = 3
)
}
\description{
Modified function to identify metabolites
}
