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
    headerbox_factory(
      title = 'Retention Time Alignment Method',
      status = 'primary',
      width = 6,
      content = tagList(
        selectInput(ns('al_method'), 'Select method to use:',
                    c('Peak Density' = 'pd',
                      'Obiwarp' = 'ow',
                      'Landmark-based' = 'lama'))
      )
    ),

    # Parameters for selected alignment method
    uiOutput(ns('al_params')),

    uiOutput(ns('al_results'))
  )
}

#' alignment Server Functions
#'
#' @noRd
mod_alignment_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    pd_params <- tagList(
      fluidRow(
        col_3(numericInput(ns('min_fraction'), HTML('Min. proportion<br/>of samples'),
                           value = 0.8, min = 0, max = 1, step = 0.05)),
        col_3(numericInput(ns('extra_peaks'), HTML('Max. extra peaks<br/>per group'),
                           value = 1, step = 1)),
        col_3(selectInput(ns('smooth'), HTML('Smoothing<br/>function'),
                          c('LOESS' = 'loess',
                            'Linear' = 'linear'))),
        col_3(uiOutput(ns('if_loess')))
      )
    )

    output$if_loess <- renderUI({
      if(input$smooth == 'loess'){
        numericInput(ns('span'), HTML('Degree of<br/>smoothing'), value = 0.2, step = 0.1)
      }
    })

    ow_params <- tagList(
      fluidRow(
        col_3(numericInput(ns('bin_size'), HTML('Bin<br/>Size'), value = 1, step = 0.1)),
        col_3(numericInput(ns('response'), HTML('Warping<br/>response'), value = 1,
                           min = 0, max = 100)),
        col_3(selectInput(ns('dist_fun'), HTML('Distance<br/>function'),
                          c("Pearson" = 'cor',
                            'Optimized' = 'cor_opt',
                            'Covariance' = 'cov',
                            'Product' = 'prd',
                            'Euclidian' = 'euc')),
              selected = 'cor_opt'),
        col_3(selectInput(ns('local_Al'), HTML('Type of<br/>alignment'),
                          c('Local' = TRUE, 'Global' = FALSE)))
      )
    )

    lama_params <- tagList(
      fluidRow(
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
        numericInput(ns('span_lama'), HTML('Degree of<br/>smoothing'), value = 0.2, step = 0.1)
      } else {
        selectInput(ns('gam_smooth'), HTML('GAM<br/>smoothing'),
                    c('tp', 'ts', 'ds', 'cr', 'cs', 'cc', 'sos', 'bs', 'ps',
                      're', 'mrf', 'gp', 'so'),
                    selected = 'tp')
      }
    })

    subset_params <- tagList(
      checkboxInput(ns('use_subset'), "Align to subset?", value = FALSE),
      uiOutput(ns('subset_opt'))
    )

    output$subset_opt <- renderUI({
      if(input$use_subset){
        tagList(
          hr(),
          fluidRow(
            col_12(
              shinyWidgets::virtualSelectInput(ns('subset_samples'),
                                               'Select samples to estimate alignment model',
                                               choices = MTandem_obj$metadata$SampleID,
                                               multiple = TRUE,
                                               showValueAsTags = TRUE)
            )
          )
        )
      } else {
        tagList()
      }
    })

    output$al_params <- renderUI({

      if(input$al_method == 'pd'){
        cont <- c(pd_params,
                  subset_params)
      } else if(input$al_method == 'ow'){
        cont <- c(ow_params,
                  subset_params)
      } else if(input$al_method == 'lama'){
        cont <- c(lama_params)
      }

      headerbox_factory(
        title = 'Method Parameters',
        width = 12,
        status = 'primary',
        content = cont
      )
    }) %>%
      bindEvent(input$al_method)

  })
}

## To be copied in the UI
# mod_alignment_ui("alignment_1")

## To be copied in the server
# mod_alignment_server("alignment_1")
