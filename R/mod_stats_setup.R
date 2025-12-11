#' stats_setup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_setup_ui <- function(id, solo = FALSE) {
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
      id = 'norm_params_box',
      content = tagList(
        fluidRow(
          col_12( uiOutput(ns('if_solo'))),
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

    if(solo){
      fluidRow(
        col_6(),
        col_6(uiOutput(ns('next_buttonsSS')))
      )
    } else {
      fluidRow(
        col_3(back_button(id = 'back_buttonSS')),
        col_3(),
        col_6(uiOutput(ns('next_buttonsSS')))
      )
    }

  )
}

#' stats_setup Server Functions
#'
#' @noRd
mod_stats_setup_server <- function(id, MTandem_obj, solo = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Waiter
    waiter_ss <- waiter::Waiter$new(id = ns('norm_params_box'),
                                    html = waiter::spin_hexdots(),
                                    color = waiter::transparent(.1))

    if(solo){
      output$if_solo <- renderUI({
        fluidRow(
          column(5,
                 fileInput(ns('stats_file'), 'Metabolite abundance file', accept = c('.csv', '.tsv'))),
          col_1(uiOutput(ns('new_file_check'))),
          column(5,
                 fileInput(ns('solo_metadata_file'), 'Metadata file', accept = c('.csv', '.tsv'))),
          col_1(uiOutput(ns('new_metadata_check')))
        )
      })
    }

    output$new_file_check <- renderUI({
      MTandem_obj$abundance_table <- load_dataframe(input$stats_file$datapath)

      if(!is.null(MTandem_obj$abundance_table)){
        HTML(as.character(span(icon("check", style = "color: green;"))))
      }
    }) %>%
      bindEvent(input$stats_file)

    output$new_metadata_check <- renderUI({
      MTandem_obj$load_metadata(input$solo_metadata_file$datapath)

      if(!is.null(MTandem_obj$metadata)){
        HTML(as.character(span(icon("check", style = "color: green;"))))
      }
    }) %>%
      bindEvent(input$solo_metadata_file)

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
                                   'mean',
                                   'max')){
        checkboxInput(ns('log_transform'),
                      'Apply generalized log transformation',
                      value = FALSE)
      }
    })

    is_norm <- reactive({
      waiter_ss$show()
      MTandem_obj$filter_and_normalize(min_perc_samples = input$prevalence,
                                       filter_method = input$filter_methods,
                                       perc_remove = input$perc_filter,
                                       norm_method = input$norm_methods,
                                       log_transform = input$log_transform)
      waiter_ss$hide()

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

      extra_id <- ''
      if(solo){
        id1 <- 'SS_univ_solo'
        id2 <- 'SS_multi_solo'
      } else {
        id1 <- 'SS_univ'
        id2 <- 'SS_multi'
      }

      if(is_norm()){
        tagList(
          other_arrow_button(id = id1,
                             label = 'Go to Univariate Analysis'),
          br(),
          other_arrow_button(id = id2,
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
