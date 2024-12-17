#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Initializing the app in the home screen
  shinyjs::show('home')

  # Change from the home screen to any of the other modes
  observeEvent({input$goHome_database; input$goDatabase}, {
    shinyjs::toggle('database_app')
    shinyjs::toggle('home')
  })

  observeEvent({input$goHome_main; input$goMain}, {
    shinyjs::toggle('main_pipeline')
    shinyjs::toggle('home')
  })

  observeEvent({input$goHome_autotuner; input$goAutotuner}, {
    shinyjs::toggle('autotuner_app')
    shinyjs::toggle('home')
  })

  observeEvent({input$goHome_annot; input$goAnnotate}, {
    shinyjs::toggle('annotate_app')
    shinyjs::toggle('home')
  })

  # Main pipeline ----
  ## Initializing data object ----
  MTandem_obj <- MetaboTandem$new()

  ## Load data ----
  mod_load_data_server("load_data_1", MTandem_obj)
  ### Next button logic in loadData
  observeEvent(input$next_buttonLD ,{
    updateTabItems(session, "sidebarID", "p_pick")
  })


  #
  # #next button logic in peak picking
  #
  # observeEvent(input$next_buttonPP ,{
  #   updateTabItems(session, "sidebarID", "align")
  # })
  # #next button logic in align
  # observeEvent(input$next_buttonSA ,{
  #   updateTabItems(session, "sidebarID", "gap")
  # })
  #
  # #backbutton logic in peak picking
  # observeEvent(input$back_buttonPP ,{
  #   updateTabItems(session, "sidebarID", "load_data")
  # })
  #
  # #back button logic in spectra align
  # observeEvent(input$back_buttonSA,{
  #   updateTabItems(session, "sidebarID", "p_pick")
  # })
  #
  # #back button logic in gap filling
  # observeEvent(input$back_buttonGF ,{
  #   updateTabItems(session, "sidebarID", "align")
  # })
  #
  #
  # #go_to_AnnotationLogic
  #
  # observeEvent(input$Go_to_Annotation ,{
  #   updateTabItems(session, "sidebarID", "dbs_annot")
  # })
  #
  #
  # #go_to_StatisticalAnalysisLogic
  # observeEvent(input$Go_to_StatisticalAnalysis ,{
  #   updateTabItems(session, "sidebarID", "stats-setup")
  # })
  #
  #
  #
  # # Running the MGF annotation module
  #
  # dbAnnotationServer('solo_annotation', new_data = TRUE)
  #
  # # Running the Autotuner server
  #
  # autotunerServer('use_autotuner')

  # Running the main pipeline mode

  ## Pre-processing modules

}
