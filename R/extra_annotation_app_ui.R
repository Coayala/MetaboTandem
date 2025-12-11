#' Annotation app User-Interface
#'
#' @import shiny
#' @import shinydashboardPlus
#' @import shinydashboard
#'
#' @noRd
annotation_app_ui <- function(){

  dashboardPage(
    app_header(home = FALSE),

    dashboardSidebar(
      fluidRow(
        column(6, align = 'center', offset = 3,
               br(),
               back_button(id = 'goHome_annot')
        )
      )
    ),

    dashboardBody(
      mod_annotation_ui("solo_annotation_1")
    )
  )

}
