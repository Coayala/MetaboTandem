#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyFiles
#' @import shinydashboardPlus
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Application UI logic

    # Using shinyjs to toogle between the home screen and the four main MetaboTandem modes
    shinyjs::useShinyjs(),

    # Using waiter
    waiter::useWaiter(),

    # Home screen
    div(id = 'home',
        style = 'display:none',
        home_ui()
    ),
    # Database management
    div(id = 'database_app',
        style = 'display:none',
        # database_appUI
    ),
    # Main pipeline
    div(id = 'main_pipeline',
        style = 'display:none',
        mod_main_pipeline_ui()
    ),
    # Autotuner mode
    div(id = 'autotuner_app',
        style = 'display:none',
        # autotunerUI('use_autotuner')
    ),

    # MGF annotation mode
    div(id = 'annotate_app',
        style = 'display:none',
        mod_annotation_ui("solo_annotation_1")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MetaboTandem"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    htmltools::htmlDependency("shinyBS",
                              version = "0.62",
                              src = "www",
                              script = "shinyBS.js",
                              stylesheet = "shinyBS.css",
                              all_files = T,
                              package = "shinyBS")
  )
}
