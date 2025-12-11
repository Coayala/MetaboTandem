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

  observeEvent({input$goHome_stats; input$goStats}, {
    shinyjs::toggle('stats_app')
    shinyjs::toggle('home')

    # shinyjs::runjs("$('a[data-value=\"stats_setup_solo\"]').tab('show');")
  })

  observeEvent({input$goHome_Metaclean; input$goMetaclean}, {
    shinyjs::toggle('metaclean_app')
    shinyjs::toggle('home')
  })

  # Main pipeline ----
  ## Initializing data object ----

  ### Object for main pipeline
  MTandem_obj <- MetaboTandem$new()
  ### Object for solo analysis
  MTandem_obj_solo <- MetaboTandem$new()
  ### Object for metaclean
  MTandem_obj_mc <- MetaboTandem$new()

  # SOLO Autotuner
  mod_autotuner_server("autotuner_1", MTandem_obj_solo)

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


  # Statistical Analysis ----
  ##  Setup ----
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

  ## Univariate ----
  mod_stats_univ_server("stats_univ_1", MTandem_obj)
  ### Back button to statistical setup
  observeEvent(input$back_buttonSU ,{
    updateTabItems(session, "sidebarID", "stats_setup")
  })
  ### Go to multi from univ
  observeEvent(input$SU_multi,{
    updateTabItems(session, "sidebarID", "stats_multi")
  })

  ## Multivariate ----
  mod_stats_multi_server("stats_multi_1", MTandem_obj)
  ### Back button statistical setup
  observeEvent(input$back_buttonSM ,{
    updateTabItems(session, "sidebarID", "stats_setup")
  })
  ### Go to univ from multi
  observeEvent(input$SM_univ,{
    updateTabItems(session, "sidebarID", "stats_univ")
  })

  # Annotations logic ----
  mod_annotation_server("annotation_1", MTandem_obj)
  ### Back button tp gap filling
  observeEvent(input$back_buttonAN ,{
    updateTabItems(session, "sidebarID", "post_proc")
  })


  # SOLO annot ----
  mod_annotation_server("solo_annotation_1", MTandem_obj_solo, solo = TRUE)


  # SOLO stats ----
  ## Setup ----
  mod_stats_setup_server("solo_stats_setup_1", MTandem_obj_solo, solo = TRUE)
  ### Go to status multivariate after setup
  observeEvent(input$SS_multi_solo,{
    updateTabItems(session, "sidebar_stats", "stats_multi_solo")
  })
  ### Go to status univariate after setup
  observeEvent(input$SS_univ_solo,{
    updateTabItems(session, "sidebar_stats", "stats_univ_solo")
  })
  ## Multivariate SOLO ----
  mod_stats_multi_server("solo_stats_multi_1", MTandem_obj_solo)
  ### Back button to statistical setup
  observeEvent(input$back_buttonSM_solo ,{
    updateTabItems(session, "sidebar_stats", "stats_setup_solo")
  })
  ### Go to univ from multi SOLO
  observeEvent(input$SM_univ_solo,{
    updateTabItems(session, "sidebar_stats", "stats_univ_solo")
  })
  ## Univariate SOLO----
  mod_stats_univ_server("solo_stats_univ_1", MTandem_obj_solo)
  ### Back button to statistical setup
  observeEvent(input$back_buttonSU_solo ,{
    updateTabItems(session, "sidebar_stats", "stats_setup_solo")
  })
  ### Go to multi from univ
  observeEvent(input$SU_multi_solo,{
    updateTabItems(session, "sidebar_stats", "stats_multi_solo")
  })

  # METACLEAN module ----

  ## Load data ----
  mod_load_data_server("mc_load_data_1", MTandem_obj_mc)
  ### Next button logic in loadData server
  observeEvent(input$next_buttonLD ,{
    updateTabItems(session, "sidebar_metaclean", "p_pick_mc")
  })

  ## Peak picking ----
  mod_peak_picking_server("mc_peak_picking_1", MTandem_obj_mc)
  ### Back button logic in peak picking server
  observeEvent(input$back_buttonPP ,{
    updateTabItems(session, "sidebar_metaclean", "load_data_mc")
  })
  ### Next button logic in peak picking server
  observeEvent(input$next_buttonPP ,{
    updateTabItems(session, "sidebar_metaclean", "align_mc")
  })


  ## Alignment ----
  mod_alignment_server("mc_alignment_1", MTandem_obj_mc)
  ### Back button logic in alignment server
  observeEvent(input$back_buttonAL ,{
    updateTabItems(session, "sidebar_metaclean", "p_pick_mc")
  })
  ### Next button logic in alignment server
  observeEvent(input$next_buttonAL ,{
    updateTabItems(session, "sidebar_metaclean", "train_mc")
  })

  ## Training ----
  mod_metaclean_server("metaclean_1", MTandem_obj_mc)

}
