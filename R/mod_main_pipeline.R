#' main_pipeline UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyalert shinyalert
#' @noRd
mod_main_pipeline_ui <- function() {
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
                 menuSubItem('Gap Filling',
                             tabName = 'gap',
                             icon = icon('fill')),
                 startExpanded = TRUE),
        menuItem('Annotation',
                 tabName = 'annot',
                 icon = icon('tags'),
                 menuSubItem('Public or custom databases',
                             tabName = 'dbs_annot',
                             icon = icon('database')),
                 menuSubItem('Using SIRIUS',
                             tabName = 'sirius_annot',
                             icon = icon('computer')),
                 startExpanded = TRUE),
        menuItem('Statistical Analysis',
                 tabName = 'stat',
                 icon = icon('chart-line'),
                 menuSubItem('Setup',
                             tabName = 'stats-setup',
                             icon = icon('tasks')),
                 menuSubItem('Multivariate analysis',
                             tabName = 'stats-mult',
                             icon = icon('chart-bar')),
                 menuSubItem('Differential expression',
                             tabName = 'diff-exp',
                             icon = icon('expand-alt')),
                 startExpanded = TRUE),
        menuItem('Results summary',
                 tabName = 'res_summ',
                 icon = icon('table')),
        menuItem('Results Download',
                 tabName = 'res',
                 icon = icon('download'),
                 menuSubItem('Pre-processing Tables',
                             tabName = 'res_preproc',
                             icon = icon('file-download')))
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
        tabItem(tabName = 'gap',
                h1('Gap Filling'),
                mod_gap_filling_ui("gap_filling_1")
        ),

        ## Annotation module
        tabItem(tabName = 'dbs_annot',
                h1('Annotation using custom or public databases')
                # dbAnnotationUI('annot_dbs')
        ),
        tabItem(tabName = 'sirius_annot',
                h1('Annotation using SIRIUS predictions')
                # siriusAnnotationUI('annot_sirius')
        ),

        ## Statistical analysis
        tabItem(tabName = 'stats-setup',
                h1('Set options for statistical analysis')
                # statsSetupUI('st_setup')
        ),
        tabItem(tabName = 'stats-mult',
                h1('Multivariate Analysis')
                # multivariateUI('multi')
        ),
        tabItem(tabName = 'diff-exp',
                h1('Differential Analysis')
                # diffExpressionUI('diffexp')
        ),

        ## Results tabs
        tabItem(tabName = 'res_summ',
                h1('Results summary')
                # summaryUI('summ')
        ),

        tabItem(tabName = 'res_preproc',
                h1('Select data to download')
                # download_ResultspreprocUI('dl_preproc')
        )
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
