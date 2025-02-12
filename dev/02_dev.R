# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Adding R6 object ----

golem::add_r6('MetaboTandem')

## Add modules ----
## Create a module infrastructure in R/
### Main pipeline
golem::add_module(name = "main_pipeline", with_test = TRUE) # Name of the module
#### Load data
golem::add_module(name = "load_data", with_test = TRUE) # Name of the module
golem::add_fct("load_data")

#### Peak picking
golem::add_module(name = "peak_picking", with_test = TRUE) # Name of the module
golem::add_fct("peak_picking")

#### Alignment
golem::add_module(name = "alignment", with_test = TRUE) # Name of the module
golem::add_fct("alignment")
golem::add_fct("correspondence")

#### Gap filling
golem::add_module(name = "gap_filling", with_test = TRUE) # Name of the module
golem::add_fct("gap_filling")

#### Statistical Analysis
golem::add_module(name = "stats_setup", with_test = TRUE) # Name of the module
golem::add_fct("extract")
golem::add_fct("normalization")
golem::add_fct("data_filter")

##### Univariate analysis
golem::add_module(name = "stats_univ", with_test = TRUE) # Name of the module
golem::add_fct("stats_univ")
##### Multivariate analysis
golem::add_module(name = "stats_multi", with_test = TRUE) # Name of the module
golem::add_fct("stats_multi")
golem::add_fct("external")


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("home_ui")
golem::add_utils("ui_helpers")
golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")
golem::add_any_file("file.json")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation
## Add packages
usethis::use_package('shinydashboard')
usethis::use_package('shinydashboardPlus')
usethis::use_package('shinyFiles')
usethis::use_package('shinyjqui')
usethis::use_package('readr')
usethis::use_package('dplyr')
usethis::use_package('tidyr')
usethis::use_package('MSnbase')

## Vignette ----
usethis::use_vignette("MetaboTandem")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
