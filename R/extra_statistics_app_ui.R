#' Statistics app User-Interface
#'
#' @import shiny
#' @import shinydashboardPlus
#' @import shinydashboard
#'
#' @noRd
statistics_app_ui <- function(){

  dashboardPage(
    app_header(home = FALSE),

    dashboardSidebar(
      sidebarMenu(
        id = 'sidebar_stats',
        fluidRow(
          column(6, align = 'center', offset = 3,
                 br(),
                 back_button(id = 'goHome_stats')
          )
        ),

        # Left side items to select the different steps of the pipeline

        menuItem('Setup',
                 tabName = 'stats_setup_solo',
                 icon = icon('tasks'),
                 selected = TRUE),
        menuItem('Multivariate analysis',
                 tabName = 'stats_multi_solo',
                 icon = icon('chart-bar')),
        menuItem('Univariate analysis',
                 tabName = 'stats_univ_solo',
                 icon = icon('expand-alt'))
      )
    ),

    dashboardBody(

      tabItems(
        ## Statistical analysis
        tabItem(tabName = 'stats_setup_solo',
                h1('Preprocess data for statistical analysis'),
                mod_stats_setup_ui("solo_stats_setup_1", solo = TRUE)
        ),
        tabItem(tabName = 'stats_multi_solo',
                h1('Multivariate Analysis'),
                mod_stats_multi_ui("solo_stats_multi_1", solo = TRUE)
        ),
        tabItem(tabName = 'stats_univ_solo',
                h1('Univariate analysis'),
                mod_stats_univ_ui("solo_stats_univ_1", solo = TRUE)
        )
      )


    )
  )

}
