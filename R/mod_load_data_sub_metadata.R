#' load_data_sub_metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_data_sub_metadata_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2('Select metadata file'),
    fileInput(ns('metadata_file'), 'Choose file', accept = c('.csv', '.tsv')),
    dataTableOutput(ns('metadata_table'))
  )
}

#' load_data_sub_metadata Server Functions
#'
#' @noRd
mod_load_data_sub_metadata_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    MTandem_obj$load_metadata(input$metadata_file$datapath)

    output$metadata_table <- renderDataTable(
      MTandem_obj$metadata,
      options = list(scrollX = TRUE,
                     dom = 'ltip')
    )

  })
}

## To be copied in the UI
# mod_load_data_sub_metadata_ui("load_data_sub_metadata_1")

## To be copied in the server
# mod_load_data_sub_metadata_server("load_data_sub_metadata_1")
