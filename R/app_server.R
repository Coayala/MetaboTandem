#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  waiter_app <- waiter::Waiter$new(html = waiter::spin_hexdots(),
                                   color = waiter::transparent(.1))

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
  ### Next button logic in loadData server
  observeEvent(input$next_buttonLD ,{
    updateTabItems(session, "sidebarID", "p_pick")
  })

  ## Peak picking ----
  mod_peak_picking_server("peak_picking_1", MTandem_obj)
  ### Back button logic in peak picking server
  observeEvent(input$back_buttonPP ,{
    updateTabItems(session, "sidebarID", "load_data")
  })
  ### Next button logic in peak picking server
  observeEvent(input$next_buttonPP ,{
    updateTabItems(session, "sidebarID", "align")
  })


  ## Alignment ----
  mod_alignment_server("alignment_1", MTandem_obj)
  ### Back button logic in alignment server
  observeEvent(input$back_buttonAL ,{
    updateTabItems(session, "sidebarID", "p_pick")
  })
  ### Next button logic in alignment server
  observeEvent(input$next_buttonAL ,{
    updateTabItems(session, "sidebarID", "post_proc")
  })


  ## Gap Filling ----
  mod_postprocessing_server("postprocessing_1", MTandem_obj)
  ### Back button logic in gap filling server
  observeEvent(input$back_buttonPOST ,{
    updateTabItems(session, "sidebarID", "align")
  })
  ### Go to statistical analysis in gap filling server
  observeEvent(input$GF_stats,{
    if(is.null(MTandem_obj$abundance_table)){
      MTandem_obj$extract_abundance_table()
    }
    updateTabItems(session, "sidebarID", "stats_setup")
  })
  ### Go to annotation in gap filling server
  observeEvent(input$GF_annot,{

    waiter_app$show()
    if(is.null(MTandem_obj$feature_spectra)){
      MTandem_obj$extract_feature_spectra()
    }
    if(is.null(MTandem_obj$feature_definitions)){
      MTandem_obj$extract_feature_definitions()
    }
    if(is.null(MTandem_obj$feature_chromatograms)){
      MTandem_obj$extract_feature_chromatograms()
    }
    waiter_app$hide()
    updateTabItems(session, "sidebarID", "dbs_annot")
  })


  # Statistical Analysis - Setup ----
  mod_stats_setup_server("stats_setup_1", MTandem_obj)
  ### Back button statistical setup
  observeEvent(input$back_buttonSS ,{
    updateTabItems(session, "sidebarID", "post_proc")
  })
  ### Go to status multivariate after setup
  observeEvent(input$SS_multi,{
    updateTabItems(session, "sidebarID", "stats_multi")
  })
  ### Go to status univariate after setup
  observeEvent(input$SS_univ,{
    updateTabItems(session, "sidebarID", "stats_univ")
  })

  # Statistical Analysis - Univariate ----
  mod_stats_univ_server("stats_univ_1", MTandem_obj)
  ### Back button statistical setup
  observeEvent(input$back_buttonSU ,{
    updateTabItems(session, "sidebarID", "stats_setup")
  })
  ### Go to statistical analysis in gap filling server
  observeEvent(input$SU_multi,{
    updateTabItems(session, "sidebarID", "stats_multi")
  })

  # Statistical Analysis - Multivariate ----
  mod_stats_multi_server("stats_multi_1", MTandem_obj)
  ### Back button statistical setup
  observeEvent(input$back_buttonSM ,{
    updateTabItems(session, "sidebarID", "stats_setup")
  })
  ### Go to statistical analysis in gap filling server
  observeEvent(input$SM_univ,{
    updateTabItems(session, "sidebarID", "stats_univ")
  })

  # Annotations logic ----
  mod_annotation_server("annotation_1", MTandem_obj)
  ### Back button tp gap filling
  observeEvent(input$back_buttonAN ,{
    updateTabItems(session, "sidebarID", "post_proc")
  })


  #Stand_alone annotation logic ----
  mod_annotation_server("solo_annotation_1", solo = TRUE)

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
