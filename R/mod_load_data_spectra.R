#' load_data_spectra UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_data_spectra_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' load_data_spectra Server Functions
#'
#' @noRd 
mod_load_data_spectra_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_load_data_spectra_ui("load_data_spectra_1")
    
## To be copied in the server
# mod_load_data_spectra_server("load_data_spectra_1")
