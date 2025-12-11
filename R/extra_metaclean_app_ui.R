#'
#' @import shiny
#' @import shinydashboardPlus
#' @import shinydashboard
#'
#' @noRd
metaclean_app_ui <- function(){

  dashboardPage(
    app_header(home = FALSE),

    dashboardSidebar(

      sidebarMenu(
        id = 'sidebar_metaclean',
        fluidRow(
          column(6, align = 'center', offset = 3,
                 br(),
                 back_button(id = 'goHome_Metaclean')
          )
        ),

        # Steps for MetaClean

        menuItem('Load Data',
                 tabName = 'load_data_mc',
                 icon = icon('upload'),
                 selected = TRUE),
        menuItem('Peak picking',
                 tabName = 'p_pick_mc',
                 icon = icon('check')),
        menuItem('Alignment & Correspondence',
                 tabName = 'align_mc',
                 icon = icon('align-center')),
        menuItem('Train MetaClean model',
                 tabName = 'train_mc',
                 icon = icon('dumbbell'))
      )
    ),

    dashboardBody(

      tabItems(

        # Load data tab

        # Change between tabs

        ## Pre-processing tabs
        tabItem(tabName = 'load_data_mc',
                h1('Load your data'),
                mod_load_data_ui("mc_load_data_1")
        ),

        tabItem(tabName = 'p_pick_mc',
                h1('Peak Picking'),
                mod_peak_picking_ui("mc_peak_picking_1")
        ),
        tabItem(tabName = 'align_mc',
                h1('Spectra alignment'),
                mod_alignment_ui("mc_alignment_1")
        ),
        tabItem(tabName = 'train_mc',
                h1('Train MetaClean model'),
                mod_metaclean_ui("metaclean_1")

        )
      )

    )
  )

}
