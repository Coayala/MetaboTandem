
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `MetaboTandem: A pipeline for the analysis of`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{MetaboTandem}` like so:

``` r
install.packages("devtools")
devtools::install_github("Coayala/MetaboTandem")
```

## Run

You can launch the application by running:

``` r
MetaboTandem::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-02-03 11:24:30 MST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> Warning in fun(libname, pkgname): mzR has been built against a different Rcpp version (1.0.13)
#> than is installed on your system (1.0.14). This might lead to errors
#> when loading mzR. If you encounter such issues, please send a report,
#> including the output of sessionInfo() to the Bioc support forum at 
#> https://support.bioconductor.org/. For details see also
#> https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
#> ℹ Loading MetaboTandem
#> Warning: replacing previous import 'shinydashboard::taskItem' by
#> 'shinydashboardPlus::taskItem' when loading 'MetaboTandem'
#> Warning: replacing previous import 'shinydashboard::dashboardHeader' by
#> 'shinydashboardPlus::dashboardHeader' when loading 'MetaboTandem'
#> Warning: replacing previous import 'shinydashboard::box' by
#> 'shinydashboardPlus::box' when loading 'MetaboTandem'
#> Warning: replacing previous import 'shinydashboard::messageItem' by
#> 'shinydashboardPlus::messageItem' when loading 'MetaboTandem'
#> Warning: replacing previous import 'shinydashboard::dashboardSidebar' by
#> 'shinydashboardPlus::dashboardSidebar' when loading 'MetaboTandem'
#> Warning: replacing previous import 'shinydashboard::dashboardPage' by
#> 'shinydashboardPlus::dashboardPage' when loading 'MetaboTandem'
#> Warning: replacing previous import 'shinydashboard::notificationItem' by
#> 'shinydashboardPlus::notificationItem' when loading 'MetaboTandem'
#> Error in c("(function (command = NULL, args = character(), error_on_status = TRUE, ", : ! System command 'Rcmd.exe' failed
```

``` r
covr::package_coverage()
#> Error: Failure in `C:/Users/Chris/AppData/Local/Temp/Rtmp6lr55c/R_LIBS4e301ba57aba/MetaboTandem/MetaboTandem-tests/testthat.Rout.fail`
#>     └─MetaboTandem (local) module(childScope$input, childScope$output, childScope, ...)
#>  35. └─base::.handleSimpleError(...)
#>  36.   └─shiny (local) h(simpleError(msg, call))
#> 
#> [ FAIL 3 | WARN 0 | SKIP 1 | PASS 112 ]
#> Error: Test failures
#> Execution halted
```
