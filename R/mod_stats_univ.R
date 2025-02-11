#' stats_univ UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_univ_ui <- function(id) {
  ns <- NS(id)
  tagList(
    headerbox_factory('Differential Analysis',
                      status = 'primary',
                      width = 12,
                      content = tagList(
                        fluidRow(
                          col_4(
                            selectizeInput(ns('group'),
                                           label = 'Choose grouping variables for differential analysis',
                                           choices = NULL),
                            actionButton(ns('refresh'), label = 'Refresh')
                          )
                          ,
                          col_4(
                            selectizeInput(ns('control'),
                                           label = 'Control treatment (baseline)',
                                           choices = NULL)
                          ),
                          col_4(
                            selectizeInput(ns('treatment'),
                                           label = 'Treatment to compare',
                                           choices = NULL)
                          )
                        ),
                        fluidRow(
                          col_6(),
                          col_6(shinyWidgets::actionBttn(ns('calculate'),
                                                         label = 'Calculate',
                                                         style = 'jelly',
                                                         color = 'primary',
                                                         size = 'sm',
                                                         block = TRUE))
                        )
                      )),
    uiOutput(ns('diff_results'))
  )
}

#' stats_univ Server Functions
#'
#' @noRd
mod_stats_univ_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



  })
}

## To be copied in the UI
# mod_stats_univ_ui("stats_univ_1")

## To be copied in the server
# mod_stats_univ_server("stats_univ_1")
