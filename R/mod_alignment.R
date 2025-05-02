#' alignment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_alignment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      headerbox_factory(
        title = HTML('Retention Time<br/>Alignment Method'),
        status = 'primary',
        width = 6,
        content = tagList(
          selectInput(ns('al_method'), 'Select method to use:',
                      c('Obiwarp' = 'ow',
                        'Peak Groups' = 'pg',
                        'Landmark-based' = 'lama'))
        )
      ),
      headerbox_factory(
        title = HTML('Correspondence<br/>Method'),
        status = 'primary',
        width = 6,
        content = tagList(
          selectInput(ns('cor_method'), 'Select method to use:',
                      c('Peak Density' = 'pd',
                        'Nearest Peak' = 'np'))
        )
      ),
      uiOutput(ns('al_params'))
    ),

    # Parameters for selected alignment method

    fluidRow(
      uiOutput(ns('al_selected_params')),
      uiOutput(ns('al_results')),
    ),



    # Buttons to move along the wizard

    fluidRow(
      col_3(back_button(id = 'back_buttonAL')),
      col_6(),
      col_3(uiOutput(ns('next_buttonAL')))
    )
  )
}

#' alignment Server Functions
#'
#' @noRd
mod_alignment_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    waiter_al <- waiter::Waiter$new(id = ns('al_params_box'),
                                    html = waiter::spin_hexdots(),
                                    color = waiter::transparent(.1))

    # Parameters for alignment and correspondence ----
    output$al_params <- renderUI({

      group_var <- MTandem_obj$get_groups()

      ## Alignment parameters ----
      pg_params <- tagList(
        fluidRow(
          col_12(h4('Aligment parameters', style = 'font-weight:bold')),
          col_3(numericInput(ns('min_fraction'), HTML('Min. proportion<br/>of samples'),
                             value = 0.8, min = 0, max = 1, step = 0.05),
                selectInput(ns('group_by'), HTML('Group samples<br/>by'),
                            choices =  group_var)),
          col_3(numericInput(ns('extra_peaks'), HTML('Max. extra peaks<br/>per group'),
                             value = 1, step = 1),
                numericInput(ns('ppm_pg'), HTML('Bin ppm<br/>expansion'),
                             value = 0, min = 0, step = 0.05)),
          col_3(numericInput(ns('bin_size_pg'), HTML('Bin<br/>Size'),
                             value = 0.25, min = 0, step = 0.05),
                selectInput(ns('smooth'), HTML('Smoothing<br/>function'),
                            c('LOESS' = 'loess',
                              'Linear' = 'linear'))),
          uiOutput(ns('if_loess'))
        )
      )

      output$if_loess <- renderUI({
        if(input$smooth == 'loess'){
          col_3(numericInput(ns('span_pg'), HTML('Degree of<br/>smoothing'),
                             value = 0.2, step = 0.1),
                selectInput(ns('family'), HTML('Smoothing<br/>method'),
                            c('gaussian', 'symmetric')))
        }
      })

      ow_params <- tagList(
        fluidRow(
          col_12(h4('Aligment parameters', style = 'font-weight:bold')),
          col_3(numericInput(ns('bin_size_ow'), HTML('Bin<br/>Size'),
                             value = 1, step = 0.1)),
          col_3(numericInput(ns('response'), HTML('Warping<br/>response'),
                             value = 1, min = 0, max = 100)),
          col_3(selectInput(ns('dist_fun'), HTML('Distance<br/>function'),
                            c("Pearson" = 'cor',
                              'Optimized' = 'cor_opt',
                              'Covariance' = 'cov',
                              'Product' = 'prd',
                              'Euclidian' = 'euc')),
                selected = 'cor_opt'),
          col_3()
        )
      )

      lama_params <- tagList(
        fluidRow(
          col_12(h4('Aligment parameters', style = 'font-weight:bold')),
          col_3(fileInput(ns('lama_file'), HTML('Landmarks<br/>file'),
                          accept = c('.csv', '.tsv'))),
          col_3(numericInput(ns('lama_mztol'), HTML('Mass<br/>tolerance'),
                             value = 1),
                numericInput(ns('lama_rttol'), HTML('RT<br/>tolerance'),
                             value = 0)),
          col_3(selectInput(ns('lama_method'), HTML('Type of<br/>alignment'),
                            c('LOESS' = 'loess', 'gam' = 'gam'),
                            selected = 'loess'),
                numericInput(ns('ppm_lama'), HTML('Allowed PPM<br/>difference'),
                             value = 20)),
          col_3(uiOutput(ns('method_extra_param')))
        )
      )

      output$method_extra_param <- renderUI({
        if(input$lama_method == 'loess'){
          numericInput(ns('span_lama'), HTML('Degree of<br/>smoothing'),
                       value = 0.2, step = 0.1)
        } else {
          selectInput(ns('gam_smooth'), HTML('GAM<br/>smoothing'),
                      c('tp', 'ts', 'ds', 'cr', 'cs',
                        'cc', 'sos', 'bs', 'ps', 're',
                        'mrf', 'gp', 'so'),
                      selected = 'tp')
        }
      })

      subset_params <- tagList(checkboxInput(ns('use_subset'), "Align to subset?",
                                             value = FALSE),
                               uiOutput(ns('subset_opt'))
      )

      ## Select samples if subset ----
      output$subset_opt <- renderUI({
        if(input$use_subset){
          tagList(
            hr(),
            fluidRow(
              col_9(
                shinyWidgets::virtualSelectInput(ns('subset_samples'),
                                                 HTML('Select samples to estimate<br/>alignment model'),
                                                 choices = MTandem_obj$metadata$SampleID,
                                                 multiple = TRUE,
                                                 showValueAsTags = TRUE)
              ),
              col_3(selectInput(ns('subset_adjust'), HTML('Adjust<br/>samples'),
                                c('previous', 'average')))
            )
          )
        } else {
          tagList()
        }
      })

      # Parameters for correspondence ----
      pd_params <- tagList(
        fluidRow(
          col_12(hr(style = "height:1px;border-width:0;color:gray;background-color:gray"),
                 h4('Correspondence parameters', style = 'font-weight:bold')),
          col_3(selectInput(ns('cor_group_by'), HTML('Group samples<br/>by'),
                            choices =  group_var),
                numericInput(ns('cor_ppm'), HTML('Bin ppm<br/>expansion'),
                             value = 0, min = 0, step = 0.01)),
          col_3(numericInput(ns('cor_bw'), HTML('Selected<br/>bandwidth'),
                             value = 30, min = 0, step = 1),
                numericInput(ns('cor_maxfeatures'), HTML('Max.<br/>features'),
                             value = 30, min = 0, step = 1)),
          col_3(numericInput(ns('cor_minfraction'), HTML('Min.<br/>fraction'),
                             value = 0.5, min = 0, max = 1, step = 0.05)),
          col_3(numericInput(ns('cor_binsize'), HTML('Bin<br/>size'),
                             value = 0.25, min = 0))
        )
      )

      np_params <- tagList(
        fluidRow(
          col_12(hr(style = "height:1px;border-width:0;color:gray;background-color:gray"),
                 h4('Correspondence parameters', style = 'font-weight:bold')),
          col_3(selectInput(ns('cor_group_by'), HTML('Group samples<br/>by'),
                            choices =  group_var),
                numericInput(ns('cor_knn'), HTML('Num. nearest<br/>neighbor'),
                             value = 0, min = 0, step = 0.01)),
          col_3(numericInput(ns('cor_mzvsrtbal'), HTML('Balance<br/>factor'),
                             value = 10, min = 0, step = 1)),
          col_3(numericInput(ns('cor_absmz'), HTML('Tolerated m/z<br/>distance'),
                             value = 0.2, min = 0, step = 0.05)),
          col_3(numericInput(ns('cor_absrt'), HTML('Tolerated RT<br/>distance'),
                             value = 15, min = 0))
        )
      )

      ## Buttons to perform alignment ----
      bottom_buttons <- tagList(
        fluidRow(
          col_6(),
          col_6(
            shinyWidgets::actionBttn(ns('align'),
                                     label = 'Perform alignment',
                                     style = 'jelly',
                                     color = 'primary',
                                     size = 'sm',
                                     block = TRUE)
          )
        )

      )


      if(input$al_method == 'pg'){
        cont <- c(pg_params,
                  subset_params)
      } else if(input$al_method == 'ow'){
        cont <- c(ow_params,
                  subset_params)
      } else if(input$al_method == 'lama'){
        cont <- c(lama_params)
      }

      if(input$cor_method == 'pd'){
        cont <- c(cont,
                  pd_params,
                  bottom_buttons)
      } else if(input$cor_method == 'np'){
        cont <- c(cont,
                  np_params,
                  bottom_buttons)
      }


      headerbox_factory(
        title = 'Method Parameters',
        width = 12,
        status = 'primary',
        id = ns('al_params_box'),
        content = cont
      )
    }) %>%
      bindEvent(input$al_method,
                input$cor_method)

    # Box for results ----
    output$al_selected_params <- renderUI(
      headerbox_factory(
        title = 'Selected Parameters',
        status = 'warning',
        width = 12,
        content = tagList(
          uiOutput(ns('params_tables'))
        )
      )
    ) %>%
      bindEvent(input$align)

    output$al_results <- renderUI(
      headerbox_factory(
        title = 'Alignment and correspondence results',
        status = 'success',
        width = 12,
        content = tagList(
          fluidRow(
            col_12(plotOutput(ns('al_figures'))),
            hr(),
            col_12(uiOutput(ns('cor_results')))
          )
        )
      )
    ) %>%
      bindEvent(input$align)


    output$params_tables <- renderUI({
      fluidRow(
        col_6(h4('Alignment parameters'),
              tableOutput(ns('al_table'))),
        col_6(h4('Correspondence parameters'),
              tableOutput(ns('cor_table')))
      )
    })

    # Table with selected parameters ----
    output$al_table <- renderTable({
      if(input$al_method == 'pg'){
        data.frame(
          Parameter = c('Method',
                        'Group samples by',
                        'Min. fraction of samples',
                        'Extra peaks',
                        'Smooth function',
                        'Bin size',
                        'Bin size expansion (ppm)'),
          value = c('Peak Groups',
                    input$group_by,
                    input$min_fraction,
                    input$extra_peaks,
                    input$smooth,
                    input$bin_size_pg,
                    input$ppm_pg
          ))
      } else if(input$al_method == 'ow'){
        data.frame(
          Parameter = c('Method',
                        'Bin Size',
                        'Warping responsiveness',
                        'Distance function'),
          value = c('Obiwarp',
                    input$bin_size_ow,
                    input$response,
                    input$dist_fun))
      } else if(input$al_method == 'lama'){
        data.frame(
          Parameter = c('Method',
                        'Warping method',
                        'm/z tolerance',
                        'RT tolerance',
                        'Max. difference between landmarks (ppm)'),
          value = c('Landmark-based',
                    input$lama_method,
                    input$lama_mztol,
                    input$lama_rttol,
                    input$ppm_lama))
      }
    },
    striped = TRUE,
    bordered = TRUE,
    width = '100%'
    ) %>%
      bindEvent(input$align)

    output$cor_table <- renderTable({
      if(input$cor_method == 'pd'){
        data.frame(
          Parameter = c('Method',
                        'Group samples by',
                        'Bin size',
                        'Bin size expansion (ppm)',
                        'Bandwidth',
                        'Min. fraction of samples',
                        'Max. features'),
          value = c('Peak Density',
                    input$cor_group_by,
                    input$cor_binsize,
                    input$cor_ppm,
                    input$cor_bw,
                    input$cor_minfraction,
                    input$cor_maxfeatures))
      } else if(input$al_method == 'np'){
        data.frame(
          Parameter = c('Method',
                        'Group samples by',
                        'm/z balance factor',
                        'Max. m/z distance',
                        'Max. RT distance',
                        'Num. nearest neighbor'),
          value = c('Nearest Peaks',
                    input$cor_group_by,
                    input$cor_mzvsrtbal,
                    input$cor_absmz,
                    input$cor_absrt,
                    input$cor_knn))
      }
    },
    striped = TRUE,
    bordered = TRUE,
    width = '100%'
    ) %>%
      bindEvent(input$align)

    # Applying alignment and correspondence ----
    has_correspondence <- reactive({
      waiter_al$show()

      notid <- showNotification('Aligning spectra...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      MTandem_obj$apply_alignment(method = input$al_method,
                                  group_by = input$group_by,
                                  bin_size_pg = input$bin_size_pg,
                                  ppm_bin = input$ppm_pg,
                                  min_fraction = input$min_fraction,
                                  extra_peaks = input$extra_peaks,
                                  smooth = input$smooth,
                                  bin_size_ow = input$bin_size_ow,
                                  span = input$span_pg,
                                  family = input$family,
                                  smooth_responsiveness = input$response,
                                  distance_function = input$dist_fun,
                                  subset_samples = input$subset_samples,
                                  subset_adjust = input$subset_adjust,
                                  lama_file = input$lama_file$datapath,
                                  lama_method = input$lama_method,
                                  span_lama = input$span_lama,
                                  ppm_lama = input$ppm_lama,
                                  tolerance_mz = input$lama_mztol,
                                  tolerance_rt = input$lama_rttol,
                                  gam_smoothing =  input$gam_smooth)

      notid <- showNotification('Defining features by correspondence...',
                                duration = NULL, closeButton = FALSE,
                                id = notid)
      MTandem_obj$apply_correspondence(method = input$cor_method,
                                       group_by = input$cor_group_by,
                                       bandwidth = input$cor_bw,
                                       bin_size = input$cor_binsize,
                                       ppm = input$cor_ppm,
                                       min_fraction = input$cor_minfraction,
                                       max_features = input$cor_maxfeatures,
                                       mzvsrtbal = input$cor_mzvsrtbal,
                                       abs_mz = input$cor_absmz,
                                       abs_rt = input$cor_absrt,
                                       knn = input$cor_knn)

      waiter_al$hide()

      xcms::hasFeatures(MTandem_obj$data)

    })

    observe({
      req(has_correspondence())
      output$al_figures <- renderPlot({

        group_colors <- setNames(
          ggpubr::get_palette(
            palette = 'Dark2',
            length(unique(MTandem_obj$data@sampleData[[input$cor_group_by]]))
          ),
          nm = unique(MTandem_obj$data@sampleData[[input$cor_group_by]])
        )

        color_vector <- group_colors[MTandem_obj$data@sampleData[[input$cor_group_by]]]

        xcms::plotAdjustedRtime(MTandem_obj$data, lwd = 3, cex = .7,
                                col = color_vector)
        legend('topleft', legend = names(group_colors), col = group_colors,
               lty = rep(1, length(group_colors)), cex = .8,
               lwd = rep(2, length(group_colors)))
      })
    }) %>%
      bindEvent(input$align)

    ## Correspondence results ----
    output$cor_results <- renderUI({
      if(has_correspondence()){
        res <- colored_text(
          paste0('Alignment completed successfully. ',
                 'A total of ', nrow(xcms::featureDefinitions(MTandem_obj$data)),
                 ' features were found after correspondence.'),
          color = 'green'
        )

      } else {
        res <- colored_text('No features were detected', color = 'red')
      }

      res
    }) %>%
      bindEvent(input$align)

    # Button to move to next step ----

    output$next_buttonAL <- renderUI({
      if(has_correspondence()){
        next_button(id = 'next_buttonAL')
      }
    }) %>%
      bindEvent(input$align)

  })
}

## To be copied in the UI
# mod_alignment_ui("alignment_1")

## To be copied in the server
# mod_alignment_server("alignment_1")
