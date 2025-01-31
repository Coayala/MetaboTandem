#' gap_filling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gap_filling_ui <- function(id) {
  ns <- NS(id)
  tagList(
    headerbox_factory(
      title = 'Gap filling parameters',
      status = 'primary',
      width = 6,
      id = ns('gf_box'),
      content = tagList(
        col_12(numericInput(ns('cores'), "Number of Cores", value = 1,
                           min = 1, max = parallel::detectCores())),
        shinyWidgets::actionBttn(
          inputId = ns('fill'),
          label = 'Apply gap filling',
          style = 'jelly',
          color = 'primary',
          size = 'sm',
          block = TRUE
        )
      )
    ),

    # Space for gap filling results
    uiOutput(ns('gap_results')),

    # Buttons to move along the wizard

    fluidRow(
      col_3(back_button(id = 'back_buttonGF')),
      col_3(),
      col_6(uiOutput(ns('next_buttonsGF')))
    )

  )
}

#' gap_filling Server Functions
#'
#' @noRd
mod_gap_filling_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$next_buttonsGF <- renderUI({
        tagList(
          other_arrow_button(id = 'GF_no_gap_annot',
                             label = 'Go to Annotation'),
          br(),
          other_arrow_button(id = 'GF_no_gap_stats',
                             label = 'Go to Statistical Analysis')
        )
    })

    output$gap_params <- renderUI({

      if(input$use_gap_filling){
        tagList(
          col_12(hr()),

        )
      }
    })

  })
}

## To be copied in the UI
# mod_gap_filling_ui("gap_filling_1")

## To be copied in the server
# mod_gap_filling_server("gap_filling_1")
