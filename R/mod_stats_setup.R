#' stats_setup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_setup_ui <- function(id) {
  ns <- NS(id)

  filter_list <- c('Interquantile range' = 'iqr',
                   'Standard Deviation' = 'sd',
                   'Median absolute deviation' = 'mad',
                   'Relative standard deviation' = 'rsd',
                   'None' = 'none')

  norm_list <- c('Global sum' = 'global',
                 'Median' = 'median',
                 'Mean' = 'mean',
                 'Max' = 'max',
                 'Variance stabilizing' = 'vsn',
                 'Cyclic LOESS' = 'cycloes',
                 'None' = 'none')

  tagList(
    headerbox_factory(
      title = HTML('Data Processing parameters'),
      status = 'primary',
      width = 12,
      content = tagList(
        fluidRow(
          col_6(
            shinyWidgets::numericInputIcon(ns('prevalence'),
                                           label = 'Prevalence percentage',
                                           value = 10,
                                           min = 0,
                                           max = 100,
                                           step = 1,
                                           icon = list(NULL, icon("percent")))
          ),
          col_6()
        ),
        hr(),
        fluidRow(
          col_6(
            selectizeInput(ns('filter_methods'),
                           label = 'Low variance filter method',
                           choices = filter_list,
                           selected = 'none')
          ),
          col_6(uiOutput(ns('show_perc')))
        ),
        hr(),
        fluidRow(
          col_6(
            selectizeInput(ns('norm_methods'),
                           label = 'Normalization methods',
                           choices = norm_list,
                           selected = 'none')
          ),
          col_6(
            uiOutput(ns('apply_log'))
          )
        ),
        fluidRow(
          col_6(),
          col_6(shinyWidgets::actionBttn(ns('process'),
                                         label = 'Continue',
                                         style = 'jelly',
                                         color = 'primary',
                                         size = 'sm',
                                         block = TRUE))
        )
      )
    ),

    uiOutput(ns('setup_results')),

    fluidRow(
      col_3(back_button(id = 'back_buttonSS')),
      col_3(),
      col_6(uiOutput(ns('next_buttonsSS')))
    )

  )
}

#' stats_setup Server Functions
#'
#' @noRd
mod_stats_setup_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$show_perc <- renderUI({
      if(input$filter_methods != 'none'){
        shinyWidgets::numericInputIcon(ns('perc_filter'),
                                       'Percentage of features to remove',
                                       value = 10,
                                       min = 1,
                                       max = 100,
                                       step = 1,
                                       icon = list(NULL, icon("percent")))
      }
    })

    output$apply_log <- renderUI({
      if(input$norm_methods %in% c('global',
                                   'median',
                                   'mean')){
        checkboxInput(ns('log_transform'),
                      'Apply generalized log transformation',
                      value = FALSE)
      }
    })

    is_norm <- reactive({
      MTandem_obj$filter_and_normalize(min_perc_samples = input$prevalence,
                                       filter_method = input$filter_methods,
                                       perc_remove = input$perc_filter,
                                       norm_method = input$norm_methods,
                                       log_transform = input$log_transform)

      !is.null(MTandem_obj$norm_abundance_table)
    })

    output$setup_results <- renderUI({
      if(is_norm()){
        headerbox_factory(
          'Data tables',
          status = 'success',
          width = 12,
          content = tagList(
            fluidRow(
              col_6(shinyWidgets::downloadBttn(ns('raw_table'),
                                               'Raw abundance table',
                                               style = 'jelly',
                                               color = 'warning',
                                               block = TRUE,
                                               size = 'xs',
                                               icon = icon('file-export'))),
              col_6(shinyWidgets::downloadBttn(ns('processed_table'),
                                               'Processed abundance table',
                                               style = 'jelly',
                                               color = 'warning',
                                               block = TRUE,
                                               size = 'xs',
                                               icon = icon('file-export')))
            )
          )
        )
      }
    })  %>%
      bindEvent(input$process)

    output$raw_table <- downloadHandler(
      filename = function(){
        paste('raw_abundances_table.tsv')
      },
      content = function(file){
        vroom::vroom_write(MTandem_obj$abundance_table, file)
      }
    )

    output$processed_table <- downloadHandler(
      filename = function(){
        paste('processed_abundances_table.tsv')
      },
      content = function(file){
        vroom::vroom_write(MTandem_obj$norm_abundance_table, file)
      }
    )

    output$next_buttonsSS <- renderUI({
      if(is_norm()){
        tagList(
          other_arrow_button(id = 'SS_univ',
                             label = 'Go to Univariate Analysis'),
          br(),
          other_arrow_button(id = 'SS_multi',
                             label = 'Go to Multivariate Analysis')
        )
      }
    }) %>%
      bindEvent(input$process)

  })
}

## To be copied in the UI
# mod_stats_setup_ui("stats_setup_1")

## To be copied in the server
# mod_stats_setup_server("stats_setup_1")
