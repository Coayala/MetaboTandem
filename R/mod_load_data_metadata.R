#' load_data_metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_data_metadata_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' load_data_metadata Server Functions
#'
#' @noRd 
mod_load_data_metadata_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_load_data_metadata_ui("load_data_metadata_1")
    
## To be copied in the server
# mod_load_data_metadata_server("load_data_metadata_1")
