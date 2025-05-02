#' main_pipeline UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyalert shinyalert
#' @noRd
mod_main_pipeline_ui <- function(id) {
  dashboardPage(

    # Header
    app_header(),

    # Sidebar content
    dashboardSidebar(
      sidebarMenu(

        # Button to go back home
        fluidRow(
          column(6, align = 'center', offset = 3,
                 shinyWidgets::actionBttn(inputId = 'goHome_main',
                                          icon = icon('house'),
                                          style = 'material-circle',
                                          color = 'success',
                                          size = 'sm'))

        ),

        id='sidebarID',

        # Left side items to select the different steps of the pipeline

        menuItem('Data pre-processing',
                 tabName = 'preproc',
                 icon = icon('cogs'),
                 menuSubItem('Load Data',
                             tabName = 'load_data',
                             icon = icon('upload')),
                 menuSubItem('Peak picking',
                             tabName = 'p_pick',
                             icon = icon('check')),
                 menuSubItem('Alignment & Correspondence',
                             tabName = 'align',
                             icon = icon('align-center')),
                 menuSubItem('(Optional) Post processing',
                             tabName = 'post_proc',
                             icon = icon('fill')),
                 startExpanded = TRUE),
        menuItem('Annotation',
                 tabName = 'annot',
                 icon = icon('tags'),
                 menuSubItem('Public or custom databases',
                             tabName = 'dbs_annot',
                             icon = icon('database')),
                 # menuSubItem('Using SIRIUS',
                 #             tabName = 'sirius_annot',
                 #             icon = icon('computer')),
                 startExpanded = TRUE),
        menuItem('Statistical Analysis',
                 tabName = 'stat',
                 icon = icon('chart-line'),
                 menuSubItem('Setup',
                             tabName = 'stats_setup',
                             icon = icon('tasks')),
                 menuSubItem('Multivariate analysis',
                             tabName = 'stats_multi',
                             icon = icon('chart-bar')),
                 menuSubItem('Univariate analysis',
                             tabName = 'stats_univ',
                             icon = icon('expand-alt')),
                 startExpanded = TRUE)
        # menuItem('Results summary',
        #          tabName = 'res_summ',
        #          icon = icon('table')),
        # menuItem('Results Download',
        #          tabName = 'res',
        #          icon = icon('download'),
        #          menuSubItem('Pre-processing Tables',
        #                      tabName = 'res_preproc',
        #                      icon = icon('file-download')))
      )
    ),

    # Body content
    dashboardBody(
      tags$head(
        tags$style(HTML("
      #sidebarItemExpanded > ul > :last-child {
        position: absolute;
        bottom: 0;
        width: 100%;
        background-color: steelblue;
      }

    "))),
      tags$head(tags$style(type = "text/css", paste0(".vscomp-dropbox {
                                                        position: absolute !important;
                                                        bottom: auto !important;
                                                        top: 100% !important;
                                                     }}"))),
      tabItems(

        # Load data tab

        # Change between tabs

        ## Pre-processing tabs
        tabItem(tabName = 'load_data',
                h1('Load your data'),
                mod_load_data_ui("load_data_1")
        ),

        tabItem(tabName = 'p_pick',
                h1('Peak Picking'),
                mod_peak_picking_ui("peak_picking_1")
        ),
        tabItem(tabName = 'align',
                h1('Spectra alignment'),
                mod_alignment_ui("alignment_1")
        ),
        tabItem(tabName = 'post_proc',
                h1('Post processing tools'),
                mod_postprocessing_ui("postprocessing_1")
        ),

        ## Annotation module
        tabItem(tabName = 'dbs_annot',
                h1('Metabolite annotation'),
                mod_annotation_ui("annotation_1")
        ),
        # tabItem(tabName = 'sirius_annot',
        #         h1('Annotation using SIRIUS predictions')
        #         # siriusAnnotationUI('annot_sirius')
        # ),

        ## Statistical analysis
        tabItem(tabName = 'stats_setup',
                h1('Preprocess data for statistical analysis'),
                mod_stats_setup_ui("stats_setup_1")
        ),
        tabItem(tabName = 'stats_multi',
                h1('Multivariate Analysis'),
                mod_stats_multi_ui("stats_multi_1")
        ),
        tabItem(tabName = 'stats_univ',
                h1('Univariate analysis'),
                mod_stats_univ_ui("stats_univ_1")
        )

        ## Results tabs
        # tabItem(tabName = 'res_summ',
        #         h1('Results summary')
        #         # summaryUI('summ')
        # ),
        #
        # tabItem(tabName = 'res_preproc',
        #         h1('Select data to download')
        #         # download_ResultspreprocUI('dl_preproc')
        # )
      )
    ),

    # Sidebar contet
    dashboardControlbar(
      br(),
      box(
        title = 'Color palette selector',
        solidHeader = TRUE,
        width = 12
        # colorPickerUI('side')
        #h2('here')
      )
    )
  )
}
