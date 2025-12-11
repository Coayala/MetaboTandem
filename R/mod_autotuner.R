#' autotuner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_autotuner_ui <- function(id) {
  ns <- NS(id)
  tagList(

    dashboardPage(
      app_header(home = FALSE),

      dashboardSidebar(
        fluidRow(
          column(6, align = 'center', offset = 3,
                 br(),
                 back_button(id = 'goHome_autotuner')
          )
        )
      ),

      dashboardBody(

        h1('AutoTuner'),

        headerbox_factory(
          title = 'Parameters',
          status = 'primary',
          width = 12,
          id = ns('autotuner_box'),
          content = tagList(
            col_12(fileInput(ns('autotuner_metadata'), 'Choose file', accept = c('.csv', '.tsv'))),
            uiOutput(ns('other_autotuner_params'))
          )
        ),

        uiOutput(ns('autotuner_results'))

      )

    )
  )
}

#' autotuner Server Functions
#'
#' @noRd
mod_autotuner_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    waiter_at <- waiter::Waiter$new(id = c(ns('signals_plot'), ns('final_parameters')),
                                    html = waiter::spin_hexdots(),
                                    color = waiter::transparent(.1))

    output$other_autotuner_params <- renderUI({
      MTandem_obj$load_metadata(input$autotuner_metadata$datapath,
                                autotuner = TRUE)

      group_var <- MTandem_obj$get_groups()

      print(group_var)

      tagList(
        col_6(selectizeInput(ns('group'),
                             'Grouping variable',
                             choices = group_var),
              numericInput(ns('lag'), 'Lag', value = 25, step = 1)),
        col_6(numericInput(ns('threshold'), 'Threshold', value = 3.1, step = 0.1),
              numericInput(ns('influence'), 'Influence', value = 0.1, step = 0.1),
              shinyWidgets::actionBttn(
                inputId = ns('get_params'),
                label = 'Get Parameters',
                style = 'jelly',
                color = 'primary',
                size = 'sm',
                block = TRUE
              ))
      )
    }) %>%
      bindEvent(input$autotuner_metadata)

    output$autotuner_results <- renderUI({

      headerbox_factory(
        title = 'AutoTuner results',
        status = 'success',
        width = 12,
        content = tagList(
          fluidRow(
            col_12(plotOutput(ns('signals_plot'))),
            col_12(tableOutput(ns('final_parameters')))
          )

        )
      )

    }) %>%
      bindEvent(sigplot())

    sigplot <- reactive({
      MTandem_obj$start_autotuner(group = input$group,
                                  lag = input$lag,
                                  threshold = input$threshold,
                                  influence = input$influence,
                                  plot = TRUE)

      grDevices::recordPlot()

    }) %>%
      bindEvent(input$get_params)

    output$signals_plot <- renderPlot({
      sigplot()
    })


    output$final_parameters <- renderTable({
      waiter_at$show()
      MTandem_obj$extract_autotuner()
      waiter_at$hide()
    },
    striped = TRUE,
    bordered = TRUE,
    width = '100%'
    ) %>%
      bindEvent(sigplot())

  })
}

## To be copied in the UI
# mod_autotuner_ui("autotuner_1")

## To be copied in the server
# mod_autotuner_server("autotuner_1")
