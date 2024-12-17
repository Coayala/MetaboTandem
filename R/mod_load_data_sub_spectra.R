#' load_data_sub_spectra UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_data_sub_spectra_ui <- function(id) {
  ns <- NS(id)
  tagList(
    waiter::use_waiter(),
    h2('Select data directory'),
    shinyDirButton(ns('datadir'), 'Choose data folder',
                   'Please select folder with data',
                   FALSE),
    br(),
    verbatimTextOutput(ns('sel_directory')),
    br(),
    selectInput(ns('format'), 'Data format', c('mzML', 'mzXML')),
    br(),
    verbatimTextOutput(ns('is_loaded')),
    actionButton(ns('load'), 'Load Data', class = 'btn-primary'),
    uiOutput(ns('next_button'))
  )
}

#' load_data_sub_spectra Server Functions
#'
#' @noRd
mod_load_data_sub_spectra_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    shinyDirChoose(input, 'datadir', roots = c('wd' = '.'), session = session)

    data_dir <- reactive({parseDirPath(roots = c('wd' = '.'), input$datadir)})

    output$sel_directory <- renderPrint(
      paste('Directory selected: ', data_dir())
    )
  })
}

## To be copied in the UI
# mod_load_data_sub_spectra_ui("load_data_sub_spectra_1")

## To be copied in the server
# mod_load_data_sub_spectra_server("load_data_sub_spectra_1")
