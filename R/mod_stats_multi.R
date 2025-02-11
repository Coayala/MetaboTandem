#' stats_multi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_multi_ui <- function(id) {
  ns <- NS(id)

  ordinations <- list('Unsupervised Methods' = c('Non-metric multidimensional scaling (NMDS)' = 'nmds',
                                                 'Principal component analysis (PCA)' = 'pca',
                                                 'Principal coordinate analysis (PCoA)' = 'pcoa'),
                      'Supervised Methods' = c('Partial least squares (PLS-DA)' = 'plsda',
                                               'Sparse partial least squares (sPLS-DA)' = 'splsda'))

  tagList(
    headerbox_factory('Ordination Analysis',
                      status = 'primary',
                      width = 12,
                      content = tagList(
                        fluidRow(
                          col_12(
                            selectizeInput(ns('analysis_method'),
                                           'Analysis method',
                                           choices = ordinations)
                          )
                        ),
                        uiOutput(ns('analysis_params')),
                        fluidRow(
                          col_6(),
                          col_6(shinyWidgets::actionBttn(ns('process'),
                                                         label = 'Continue',
                                                         style = 'jelly',
                                                         color = 'primary',
                                                         size = 'sm',
                                                         block = TRUE))
                        )
                      )),

    uiOutput(ns('multi_results')),

    fluidRow(
      col_3(back_button(id = 'back_buttonSM')),
      col_3(),
      col_6(uiOutput(ns('next_buttonsSM')))
    )

  )
}

#' stats_multi Server Functions
#'
#' @noRd
mod_stats_multi_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$analysis_params <- renderUI({
      if(input$analysis_method %in% c('nmds', 'pcoa')){
        fluidRow(
          col_6(
            selectizeInput(ns('distance'),
                           'Dissimilarity distance',
                           choices = c('bray',
                                       'euclidean',
                                       'manhattan',
                                       'chord',
                                       'hellinger'))
          )
        )
      }
    })

    has_ordination <- reactive({
      MTandem_obj$calculate_ordination(method = input$analysis_method,
                                       distance = input$distance)

      !is.null(MTandem_obj$ordination)
    }) %>%
      bindEvent(input$process)

    output$multi_results <- renderUI({
      group_var <- MTandem_obj$get_groups()
      headerbox_factory('Ordination Analysis',
                        status = 'primary',
                        width = 12,
                        content = tagList(
                          col_3(
                            h5(tags$strong('Figure options')),
                            checkboxInput(ns('add_smp'),
                                          'Add sample names',
                                          value = FALSE),
                            checkboxInput(ns('add_features'),
                                          'Add feature loadings',
                                          value = FALSE),
                            checkboxInput(ns('add_ellipse'),
                                          'Add ellipse',
                                          value = FALSE),
                            selectizeInput(ns('color_by'),
                                           'Color by',
                                           choices = group_var),
                            uiOutput(ns('color_picker')),
                            shinyWidgets::actionBttn(ns('update'),
                                                     label = 'Update plot',
                                                     style = 'jelly',
                                                     color = 'primary',
                                                     size = 'sm',
                                                     block = TRUE)
                          ),
                          col_9(plotOutput(ns('ord_plot')),
                                verbatimTextOutput(ns('test')))
                        ))
    }) %>%
      bindEvent(input$process)

    treatments <- reactive({
      paste0('color_', unique(MTandem_obj$metadata[[input$color_by]]))
    })

    output$color_picker <- renderUI({

      purrr::map(treatments(), function(x){
        label <- stringr::str_remove(x, 'color_')
        colorpicker_factory(id = ns(x), label = label)
      })
    }) %>%
      bindEvent(input$color_by)


    group_colors <- reactive({
      colors <- purrr::map_chr(treatments(), ~purrr::`%||%`(input[[.x]], ""))
      treatments <- stringr::str_remove(treatments(), 'color_')
      names(colors) <- treatments
      colors
    }) %>%
      bindEvent(input$process,
                input$update,
                input$color_by)


    output$test <- renderText({
      group_colors()
    }) %>%
      bindEvent(input$update)


    output$ord_plot <- renderPlot({
      req(group_colors())
      if(has_ordination()){

        MTandem_obj$plot_ordination(group_by = input$color_by,
                                    add_sample_names = input$add_smp,
                                    add_variables = input$add_features,
                                    add_ellipse = input$add_ellipse,
                                    color_vector = group_colors())
      }
    }) %>%
      bindEvent(input$process,
                input$update)

    output$next_buttonsSM <- renderUI({
      if(has_ordination()){
        tagList(
          other_arrow_button(id = 'SM_univ',
                             label = 'Go to Univariate Analysis')
        )
      }
    }) %>%
      bindEvent(input$process)

  })
}

## To be copied in the UI
# mod_stats_multi_ui("stats_multi_1")

## To be copied in the server
# mod_stats_multi_server("stats_multi_1")
