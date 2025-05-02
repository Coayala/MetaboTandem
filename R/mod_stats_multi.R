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

  tagList(
    headerbox_factory('Clustering analysis',
                      status = 'primary',
                      width = 12,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      id = 'cluster_box',
                      content = tagList(
                        uiOutput(ns('cluster'))
                      )),

    uiOutput(ns('cluster_results')),

    headerbox_factory('Ordination Analysis',
                      status = 'primary',
                      width = 12,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      id = 'ordination_box',
                      content = tagList(
                        uiOutput(ns('ordination'))
                      )),

    uiOutput(ns('multi_results')),

    headerbox_factory('PERMANOVA',
                      status = 'primary',
                      width = 12,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      id = 'permanova_box',
                      content = tagList(
                        uiOutput(ns('permanova'))
                      )),

    uiOutput(ns('perma_results')),

    fluidRow(
      col_3(back_button(id = 'back_buttonSM')),
      col_3(),
      col_6(
        # uiOutput(ns('next_buttonsSM'))
        other_arrow_button(id = 'SM_univ',
                           label = 'Go to Univariate Analysis')
      )
    )

  )
}

#' stats_multi Server Functions
#'
#' @noRd
mod_stats_multi_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Waiter
    waiter_sm <- waiter::Waiter$new(id = c(ns('cluster_box'),
                                           ns('ordination_box'),
                                           ns('permanova_box')),
                                    html = waiter::spin_hexdots(),
                                    color = waiter::transparent(.1))

    # Clustering server ----

    output$cluster <- renderUI({
      tagList(
        fluidRow(
          col_6(
            selectizeInput(ns('distance_cluster'),
                           'Distance method',
                           choices = c('euclidean',
                                       'manhattan',
                                       'minkiwski'),
                           selected = 'euclidean'),
            checkboxInput(ns('add_kmeans'), 'Add k-means clustering',
                          value = FALSE)
          ),
          col_6(
            selectizeInput(ns('alg_cluster'),
                           'Algorithm cluster',
                           choices = c('ward.D2',
                                       'average',
                                       'single',
                                       'complete'),
                           selected = 'ward.D2'),
            uiOutput(ns('use_k'))
          )
        ),
        fluidRow(
          col_6(),
          col_6(
            shinyWidgets::actionBttn(ns('process_cluster'),
                                     label = 'Go',
                                     style = 'jelly',
                                     color = 'primary',
                                     size = 'sm',
                                     block = TRUE)
          )
        )
      )
    })

    output$use_k <- renderUI({
      if(input$add_kmeans){
        numericInput(ns('k_value'), 'Selected k',
                     value = 1, min = 1, step = 1)
      }
    })

    output$cluster_results <- renderUI({
      group_var <- MTandem_obj$get_groups()
      headerbox_factory('Clustering Results',
                        status = 'success',
                        width = 12,
                        content = tagList(
                          col_3(
                            h5(tags$strong('Figure options')),
                            selectizeInput(ns('color_by_clust'),
                                           'Color by',
                                           choices = group_var),
                            uiOutput(ns('color_picker_clust')),
                            shinyWidgets::actionBttn(ns('update_cluster'),
                                                     label = 'Update plot',
                                                     style = 'jelly',
                                                     color = 'primary',
                                                     size = 'sm',
                                                     block = TRUE)
                          ),
                          col_9(plotOutput(ns('cluster_plot')))
                        ))
    }) %>%
      bindEvent(input$process_cluster)

    pickers_clust <- reactive({
      val <- paste0('color_clust_', unique(MTandem_obj$metadata[[input$color_by_clust]]))
      purrr::map(val, function(x){
        label <- stringr::str_remove(x, 'color_clust_')
        colorpicker_factory(id = ns(x), label = label)
      })
    })

    output$color_picker_clust <- renderUI({
      pickers_clust()
    })

    group_colors_clust <- reactive({
      req(pickers_clust())
      from_picker <- unlist(as.character(pickers_clust()))
      init_colors <- stringr::str_extract(from_picker, '#.{6}')

      tr <- unique(MTandem_obj$metadata[[input$color_by_clust]])
      input_val <- paste0('color_clust_', unique(MTandem_obj$metadata[[input$color_by_clust]]))
      colors <- purrr::map2_chr(input_val, init_colors, ~purrr::`%||%`(input[[.x]], .y))
      names(colors) <- tr
      colors
    }) %>%
      bindEvent(input$process_cluster,
                input$update_cluster,
                input$color_by_clust)


    output$cluster_plot <- renderPlot({

      if(input$add_kmeans){
        kval <- input$k_value
      } else {
        kval <- NULL
      }
      waiter_sm$show()
      notid <- showNotification('Clustering samples...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      cluster_plot <- MTandem_obj$calculate_clustering(
        distance = input$distance_cluster,
        cluster_algorithm = input$alg_cluster,
        add_kmeans = input$add_kmeans,
        k = kval,
        color_by = input$color_by_clust,
        plot = TRUE,
        color_vector = group_colors_clust()
      )
      waiter_sm$hide()

      cluster_plot

    }) %>%
      bindEvent(input$process_cluster,
                input$update_cluster)


    # Ordination server ----
    ordinations <- list('Unsupervised Methods' = c('Non-metric multidimensional scaling (NMDS)' = 'nmds',
                                                   'Principal component analysis (PCA)' = 'pca',
                                                   'Principal coordinate analysis (PCoA)' = 'pcoa'),
                        'Supervised Methods' = c('Partial least squares (PLS-DA)' = 'plsda',
                                                 'Sparse partial least squares (sPLS-DA)' = 'splsda'))

    output$ordination <- renderUI({
      tagList(
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
                                         label = 'Go',
                                         style = 'jelly',
                                         color = 'primary',
                                         size = 'sm',
                                         block = TRUE))
        )
      )
    })

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
      waiter_sm$show()
      notid <- showNotification('Calculating ordination...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      MTandem_obj$calculate_ordination(method = input$analysis_method,
                                       distance = input$distance)

      waiter_sm$hide()

      !is.null(MTandem_obj$ordination)
    }) %>%
      bindEvent(input$process)

    observe({
      req(has_ordination())
      output$multi_results <- renderUI({
        group_var <- MTandem_obj$get_groups()
        headerbox_factory('Ordination Results',
                          status = 'success',
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
                            col_9(plotOutput(ns('ord_plot')))
                          ))
      })
    }) %>%
      bindEvent(input$process)

    pickers <- reactive({
      val <- paste0('color_', unique(MTandem_obj$metadata[[input$color_by]]))
      purrr::map(val, function(x){
        label <- stringr::str_remove(x, 'color_')
        colorpicker_factory(id = ns(x), label = label)
      })
    })

    output$color_picker <- renderUI({
      pickers()
    })

    group_colors <- reactive({
      req(pickers())
      from_picker <- unlist(as.character(pickers()))
      init_colors <- stringr::str_extract(from_picker, '#.{6}')

      tr <- unique(MTandem_obj$metadata[[input$color_by]])
      input_val <- paste0('color_', unique(MTandem_obj$metadata[[input$color_by]]))
      colors <- purrr::map2_chr(input_val, init_colors, ~purrr::`%||%`(input[[.x]], .y))
      names(colors) <- tr
      colors
    }) %>%
      bindEvent(input$process,
                input$update,
                input$color_by)

    output$ord_plot <- renderPlot({
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

    # PERMANOVA server ----

    output$permanova <- renderUI({
      group_var <- MTandem_obj$get_groups()
      tagList(
        fluidRow(
          col_6(
            shinyWidgets::virtualSelectInput(ns('perma_variables'),
                                             'Select variables',
                                             choices = group_var,
                                             multiple = TRUE,
                                             showValueAsTags = TRUE),
            selectizeInput(ns('perma_block'),
                           '(Optional) Blocking (strata) variable',
                           choices = c(group_var, 'None'),
                           selected = 'None'),
            shinyWidgets::prettySwitch(
              inputId = ns('use_interaction'),
              label = 'Use interaction among variables',
              status = "success",
              fill = FALSE,
              inline = TRUE)
          ),
          col_6(
            selectizeInput(ns('distance_perma'),
                           'Dissimilarity distance',
                           choices = c('bray',
                                       'euclidean',
                                       'manhattan',
                                       'chord',
                                       'hellinger')),
            selectizeInput(ns('assess'), 'Assess',
                           choices = c('Model' = 'model',
                                       'Individual terms' = 'terms'))
          )
        ),
        fluidRow(
          col_6(),
          col_6(shinyWidgets::actionBttn(ns('process_perma'),
                                         label = 'Go',
                                         style = 'jelly',
                                         color = 'primary',
                                         size = 'sm',
                                         block = TRUE))
        )
      )
    })

    output$perma_results <- renderUI({
      headerbox_factory('Permanova Results',
                        status = 'success',
                        width = 12,
                        content = tagList(
                          col_12(uiOutput(ns('print_formula'))),
                          col_12(verbatimTextOutput(ns('permanova_result')))
                        ))
    }) %>%
      bindEvent(input$process_perma)

    output$print_formula <- renderUI({
      vars <- paste0(input$perma_variables, collapse = '+')

      if(input$use_interaction){
        vars <- paste0('abundance_table ~ (', vars, ')^2')
      } else {
        vars <- paste0('abundance_table ~ ', vars)
      }

      colored_text(paste0('PERMANOVA Formula was: ', vars), 'green')
    }) %>%
      bindEvent(input$process_perma)


    output$permanova_result <- renderPrint({
      waiter_sm$show()
      notid <- showNotification('Calculating PERMANOVA...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      if(input$perma_block == 'None'){
        res <- MTandem_obj$calculate_permanova(vars = input$perma_variables,
                                               distance = input$distance_perma,
                                               assess = input$assess,
                                               use_interaction = input$use_interaction)
      } else {
        res <- MTandem_obj$calculate_permanova(vars = input$perma_variables,
                                               distance = input$distance_perma,
                                               assess = input$assess,
                                               use_interaction = input$use_interaction,
                                               strata = input$perma_block)
      }
      waiter_sm$hide()

      res
    }) %>%
      bindEvent(input$process_perma)

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
