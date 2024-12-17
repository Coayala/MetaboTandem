#' peak_picking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_peak_picking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    headerbox_factory(
      title = 'Peak Picking Method',
      status = 'info',
      width = 6,
      content = tagList(
        selectInput(ns('pp_method'), 'Select method to use:',
                    c('centWave' = 'cw',
                      'Matched Filter' = 'mf',
                      'Massifquant' = 'mq'))
      )
    ),

    # Parameters for selected PP method
    uiOutput(ns('pp_params')),

    headerbox_factory(
      title = '',
      status = 'success',
      width = 12,
      content = tagList(
        uiOutput(ns('plot_pos')),
        uiOutput(ns('table_pos')),
        fluidRow(
          column(4, align = 'center', offset = 3,
                 shinyWidgets::actionBttn(ns('test'),
                                          label = 'Test',
                                          style = 'jelly',
                                          color = 'warning',
                                          size = 'sm',
                                          block = TRUE)
          )
        ),
        br(), br(),
        fluidRow(
          column(4, align = 'center', offset = 3,
                 shinyWidgets::actionBttn(ns('pick'),
                                          label = 'Pick peaks',
                                          style = 'jelly',
                                          color = 'primary',
                                          size = 'sm',
                                          block = TRUE)

          )
        ),br(),

        #NextButton
        uiOutput(ns('next_buttonPP')),

        #BackButton

        actionButton(inputId = 'back_buttonPP',
                     label = 'Back',
                     icon = icon('arrow-left')),

        fluidRow(
          verbatimTextOutput(ns('has_peaks'))
        )

      )
    )
  )
}

#' peak_picking Server Functions
#'
#' @noRd
mod_peak_picking_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_peak_picking_ui("peak_picking_1")

## To be copied in the server
# mod_peak_picking_server("peak_picking_1")
