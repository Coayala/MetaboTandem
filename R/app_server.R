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

  ##Next button logic in loadData
  observeEvent(input$next_button ,{
    updateTabItems(session, "sidebarID", "p_pick")
  })

  #next button logic in peak picking

  observeEvent(input$next_buttonPP ,{
    updateTabItems(session, "sidebarID", "align")
  })
  #next button logic in align
  observeEvent(input$next_buttonSA ,{
    updateTabItems(session, "sidebarID", "gap")
  })

  #backbutton logic in peak picking
  observeEvent(input$back_buttonPP ,{
    updateTabItems(session, "sidebarID", "load_data")
  })

  #back button logic in spectra align
  observeEvent(input$back_buttonSA,{
    updateTabItems(session, "sidebarID", "p_pick")
  })

  #back button logic in gap filling
  observeEvent(input$back_buttonGF ,{
    updateTabItems(session, "sidebarID", "align")
  })


  #go_to_AnnotationLogic

  observeEvent(input$Go_to_Annotation ,{
    updateTabItems(session, "sidebarID", "dbs_annot")
  })


  #go_to_StatisticalAnalysisLogic
  observeEvent(input$Go_to_StatisticalAnalysis ,{
    updateTabItems(session, "sidebarID", "stats-setup")
  })



  # Running the MGF annotation module

  dbAnnotationServer('solo_annotation', new_data = TRUE)

  # Running the Autotuner server

  autotunerServer('use_autotuner')

  # Running the main pipeline mode

  ## Pre-processing modules

  data <- load_dataServer('load_data')
  data_cent <- peakPickingServer('p_pick', data)
  data_grouped <- alignSpectraServer('align', data$metadata, data_cent)
  data_gap_filled <- gapFillingServer('gap', data_grouped)

  ## Optional color picker function

  user_colors <- colorPickerServer('side', data$metadata)

  ## Result server for pre-processing
  download_ResultspreprocServer('dl_preproc', data_gap_filled)

  ## Annotation
  dbAnnotationServer('annot_dbs', data_gap_filled)
  siriusAnnotationServer('annot_sirius', data_gap_filled)

  ## Statistical analysis module
  norm_df <- stastSetupServer('st_setup', data_gap_filled)
  multivariateServer('multi', norm_df, data$metadata, user_colors)
  diffExpressionServer('diffexp', norm_df, data$metadata)

  ## Summary results module
  summaryServer('summ', data_gap_filled, data_cent)
}
