#' Homepage header
#'
#' @description Header for the home
#'
#' @import shiny
#' @import shinydashboardPlus
#' @import shinydashboard
#'
#' @noRd
app_header <- function(home = FALSE){
  dashboardHeader(
    title = tagList(
      # TODO logo is not showing
      span(class = "logo-lg", "MetaboTandem"),
      img(src = "www/hex-MetaboTandem.png")),

    # Notification menu for adding Github and User guide link
    dropdownMenu(
      type = 'notifications',
      icon = icon('question-circle'),
      headerText = 'Help',

      notificationItem('Github Repository',
                       icon = icon('github'),
                       href = 'https://github.com/Coayala/MetaboTandem'),
      notificationItem('User Guide',
                       icon = icon('file'),
                       href = 'https://github.com/Coayala/MetaboTandem')
    ),
    leftUi = tagList(
      if(home){
        h4('Welcome to MetaboTandem', style = "color:white")
      } else {
        h4()
      }

    )
  )
}


#' Homepage User-Interface
#'
#' @import shiny
#' @import shinydashboardPlus
#' @import shinydashboard
#'
#' @noRd
home_ui <- function(){
  dashboardPage(
    #md = TRUE,

    app_header(home = TRUE),

    # Sidebar content
    dashboardSidebar(),

    # Body content
    dashboardBody(
      fluidRow(
        # Box for Autotuner
        box(title = 'Estimate Parameters',
            headerBorder = FALSE,
            background = 'purple',
            collapsible = FALSE,
            closable = FALSE,
            gradient = TRUE,
            footer = p('Use Autotuner to estimate the best parameters for your run',
                       style = 'color:gray'),
            fluidRow(
              column(6, align = 'center', offset = 3,
                     shinyWidgets::actionBttn(inputId = 'goAutotuner',
                                              icon = icon('lightbulb'),
                                              style = 'jelly',
                                              size = 'lg'))
            )
        ),

        # Box for the main pipeline
        box(title = 'Full Data analysis',
            headerBorder = FALSE,
            background = 'light-blue',
            collapsible = FALSE,
            closable = FALSE,
            gradient = TRUE,
            footer = p('Run the full analysis starting from mas spectra data',
                       style = 'color:gray'),
            fluidRow(
              column(6, align = 'center', offset = 3,
                     shinyWidgets::actionBttn(inputId = 'goMain',
                                              icon = icon('chart-simple'),
                                              style = 'jelly',
                                              size = 'lg'))
            )
        ),

        # Box for manage databases
        box(title = 'Manage Databases',
            headerBorder = FALSE,
            background = 'olive',
            collapsible = FALSE,
            closable = FALSE,
            gradient = TRUE,
            footer = p('Create custom database or download public databases',
                       style = 'color:gray'),
            fluidRow(
              column(6, align = 'center', offset = 3,
                     shinyWidgets::actionBttn(inputId = 'goDatabase',
                                              icon = icon('database'),
                                              color = 'default',
                                              style = 'jelly',
                                              size = 'lg'))
              #this is were box is made active to go to database
            )
        ),

        # Box for the annotate MGF
        box(title = 'Annotate MGF',
            headerBorder = FALSE,
            background = 'maroon',
            collapsible = FALSE,
            closable = FALSE,
            gradient = TRUE,
            footer = p('Annotate MS2 spectra using custom or public databases',
                       style = 'color:gray'),
            fluidRow(
              column(6, align = 'center', offset = 3,
                     shinyWidgets::actionBttn(inputId = 'goAnnotate',
                                              icon = icon('magnifying-glass'),
                                              style = 'jelly',
                                              size = 'lg'))
            )
        )
      )
    )
  )
}

