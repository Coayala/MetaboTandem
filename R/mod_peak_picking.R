#' peak_picking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @noRd
mod_peak_picking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      headerbox_factory(
        title = 'Peak Picking Method',
        status = 'primary',
        width = 6,
        content = tagList(
          selectInput(ns('pp_method'), 'Select method to use:',
                      c('centWave' = 'cw',
                        'Matched Filter' = 'mf',
                        'Massifquant' = 'mq'))
        )
      ),

      # Parameters for selected PP method
      uiOutput(ns('pp_params'))
    ),
    # Resulst
    fluidRow(
      uiOutput(ns('pp_selected_params')),
      uiOutput(ns('pp_results')),
    ),



    # Pop-up window
    shinyBS::bsModal(
      ns('test_popup'), 'Testing parameters', trigger = ns('test'), size = 'large',
      fluidRow(
        col_4(
          h4('Retention time'),
          numericInput(ns('rtr_min'),
                       'RT min',
                       value = 0,
                       min = 0,
                       max = 20*60,
                       step = 1),
          numericInput(ns('rtr_max'),
                       'RT max',
                       value = 240,
                       min = 0,
                       max = 20*60,
                       step = 1),
          hr(),
          h4('m/z'),
          numericInput(ns('mzr_min'),
                       'RT min',
                       value = 100,
                       min = 0,
                       max = 1200,
                       step = 0.1),
          numericInput(ns('mzr_max'),
                       'RT min',
                       value = 300,
                       min = 0,
                       max = 1200,
                       step = 0.1),
          shinyWidgets::actionBttn(ns('gen_plot'),
                                   label = 'Generate',
                                   style = 'jelly',
                                   color = 'primary',
                                   size = 'xs',
                                   block = TRUE)
        ),
        col_8(
          plotOutput(ns('pp_test_plot'))
        )
      )
    ),

    # Buttons to move along the wizard

    fluidRow(
      col_3(back_button(id = 'back_buttonPP')),
      col_6(),
      col_3(uiOutput(ns('next_buttonPP')))
    )
  )
}

#' peak_picking Server Functions
#'
#' @noRd
mod_peak_picking_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    waiter_pp <- waiter::Waiter$new(id = c(ns('pp_test_plot'), ns('pp_param_box')),
                                    html = waiter::spin_hexdots(),
                                    color = waiter::transparent(.1))

    # Method parameters ----
    cw_params <- tagList(
      fluidRow(
        col_3(numericInput(ns('ppm'), 'Ppm threshold', value = 25),
              numericInput(ns('snt'), 'Signal-to-noise threshold', value = 3),
              numericInput(ns('cores'), "Number of Cores", value = 1,
                           min = 1, max = parallel::detectCores())),
        col_3(numericInput(ns('p_width_min'), 'Min. peak width', value = 20),
              numericInput(ns('pf_k'), 'Number of peaks for pre-filtering', value = 3, step = 1)),
        col_3(numericInput(ns('noise'), 'Noise threshold', value = 1e6),
              numericInput(ns('mz_diff'), 'Mass difference for overlay peaks', value = 0.01)),
        col_3(numericInput(ns('p_width_max'), 'Max. peak width', value = 50),
              numericInput(ns('pf_i'), 'Min. intensity for prefiltering', value = 100))
      )
    )

    mf_params <- tagList(
      fluidRow(
        col_6(numericInput(ns('bin'), 'Bin size', value = 0.1),
              numericInput(ns('sigma'), 'sigma', value = 12.72),
              numericInput(ns('steps'), 'Number of bins to be merged', value = 2, step = 1)),
        col_6(numericInput(ns('fwhm'), 'fwhm', value = 30),
              numericInput(ns('max'), 'Max. peaks per slice', value = 10, step = 1))
      )
    )

    # Buttons to perform peak picking ----
    bottom_buttons <- tagList(
      checkboxInput(ns('refine'), "Perform peak refinement", value = FALSE),
      uiOutput(ns('refine_params')),
      fluidRow(
        col_6(
          shinyWidgets::actionBttn(ns('test'),
                                   label = 'Test',
                                   style = 'jelly',
                                   color = 'warning',
                                   size = 'sm',
                                   block = TRUE)
        ),
        col_6(
          shinyWidgets::actionBttn(ns('pick'),
                                   label = 'Pick peaks',
                                   style = 'jelly',
                                   color = 'primary',
                                   size = 'sm',
                                   block = TRUE)
        )
      )

    )

    # Parameters box ----
    output$pp_params <- renderUI({

      if(input$pp_method != 'mf'){
        cont <- c(cw_params,
                  bottom_buttons)
      } else {
        cont <- c(mf_params,
                  bottom_buttons)
      }

      headerbox_factory(
        title = 'Method Parameters',
        width = 12,
        status = 'primary',
        id = ns('pp_param_box'),
        content = cont
      )
    }) %>%
      bindEvent(input$pp_method)

    # Modal if testing parameters ----
    observeEvent(input$test,{
      shinyBS::toggleModal(session = session, modalId = 'test_popup', toggle = 'open')
    })

    output$pp_test_plot <- renderPlot({
      waiter_pp$show()
      if(input$mzr_max > input$mzr_min && input$rtr_max > input$rtr_min){
        test_peak_picking(MTandem_obj$data,
                          p_width = c(input$p_width_min, input$p_width_max),
                          mz_range = c(input$mzr_min, input$mzr_max),
                          rt_range = c(input$rtr_min, input$rtr_max),
                          snt = input$snt,
                          noise = input$noise,
                          prefilter = c(input$pf_k, input$pf_i),
                          cores = input$cores)
      } else {
        plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
             xaxt = "n", yaxt = "n")

        text(x = 5,y = 5,"ERROR: Please define RT and m/z boundaries properly")
      }
      waiter_pp$hide()
    }) %>%
      bindEvent(input$gen_plot)


    # Parameters for refinement ----
    output$refine_params <- renderUI({
      if(input$refine){
        tagList(
          hr(),
          fluidRow(
            col_3(numericInput(ns('expand_rt'), 'Expand retention time range',
                               value = 2, step = 0.1)),
            col_3(numericInput(ns('expand_mz'), HTML('Expand<br/>m/z range'),
                               value = 0, step = 0.1)),
            col_3(numericInput(ns('ppm_refine'), 'PPM for refinement',
                               value = 10)),
            col_3(numericInput(ns('min_prop'), 'Minimum proportion',
                               value = 0.75, min = 0, max = 1))
          )
        )
      } else {
        tagList()
      }
    })

    # Box for results ----
    output$pp_selected_params <- renderUI(
      headerbox_factory(
        title = 'Selected Parameters',
        status = 'warning',
        width = 6,
        content = tagList(
          fluidRow(
            col_12(tableOutput(ns('params_table')))
          )
        )
      )
    ) %>%
      bindEvent(input$pick)

    # Box for results ----
    output$pp_results <- renderUI(
      headerbox_factory(
        title = 'Peak picking results',
        status = 'success',
        width = 6,
        id = 'pp_res_box',
        content = tagList(
          fluidRow(
            col_12(tableOutput(ns('chrompeaks_table')),
                   uiOutput(ns('has_peaks')))
          )
        )
      )
    ) %>%
      bindEvent(input$pick)

    # Table with selected parameters ----

    params_table <- reactive({
      if(input$pp_method == 'cw'){
        data.frame(Parameter = c('Method',
                                 'Tolerated m/z deviation (ppm)',
                                 'Noise threshold',
                                 'Signal-to-noise ratio threshold',
                                 'Peak width',
                                 'Mass difference for overlay peaks',
                                 'Number of peaks for prefiltering',
                                 'Minimum intensity for prefiltering'),
                   value = c('centWave',
                             input$ppm,
                             input$noise,
                             input$snt,
                             paste0(input$p_width_min, '-', input$p_width_max),
                             input$mz_diff,
                             input$pf_k,
                             input$pf_i))
      } else if(input$pp_method == 'mf'){
        data.frame(Parameter = c('Method',
                                 'Bin size',
                                 'Standard deviation of model peak (sigma)',
                                 'Number of bins to be merged',
                                 'Full width at half maximum of matched filtration (fwhm)',
                                 'Max number of peaks per slice'),
                   value = c('Matched Filter',
                             input$bin,
                             input$sigma,
                             input$steps,
                             input$fwhm,
                             input$max))
      } else if(input$pp_method == 'mq'){
        data.frame(Parameter = c('Method',
                                 'Tolerated m/z deviation (ppm)',
                                 'Noise threshold',
                                 'Signal-to-noise ratio threshold',
                                 'Peak width',
                                 'Mass difference for overlay peaks',
                                 'Number of peaks for prefiltering',
                                 'Minimum intensity for prefiltering'),
                   value = c('Massifquant',
                             input$ppm,
                             input$noise,
                             input$snt,
                             paste0(input$p_width_min, '-', input$p_width_max),
                             input$mz_diff,
                             input$pf_k,
                             input$pf_i))
      }
    })

    output$params_table <- renderTable({
      params_table()
    },
    striped = TRUE,
    bordered = TRUE,
    width = '100%'
    ) %>%
      bindEvent(input$pick)

    # Applying peak picking and counting detected peaks ----

    has_peaks <- reactive({
      req(params_table())
      waiter_pp$show()
      notid <- showNotification('Applying peak picking...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      MTandem_obj$apply_peak_picking(method = input$pp_method,
                                     p_width = c(input$p_width_min, input$p_width_max),
                                     snt = input$snt,
                                     noise = input$noise,
                                     ppm = input$ppm,
                                     prefilter = c(input$pf_k, input$pf_i),
                                     mz_diff = input$mz_diff,
                                     bin = input$bin,
                                     fwhm = input$fwhm,
                                     sigma = input$sigma,
                                     max = input$max,
                                     steps = input$steps,
                                     cores = input$cores)

      if(input$refine){
        notid <- showNotification('Refining peaks...',
                                  duration = NULL, closeButton = FALSE,
                                  id = notid)

        MTandem_obj$apply_peak_refinement(expand_rt = input$expand_rt,
                                          expand_mz = input$expand_mz,
                                          ppm = input$ppm_refine,
                                          min_prop = input$min_prop)
      }

      waiter_pp$hide()


      xcms::hasChromPeaks(MTandem_obj$data)
    })

    observe({
      req(has_peaks())
      output$chrompeaks_table <- renderTable({
        xcms::chromPeaks(MTandem_obj$data) %>%
          tidyr::as_tibble() %>%
          dplyr::group_by(sample) %>%
          dplyr::count() %>%
          cbind(SampleID = xcms::phenoData(MTandem_obj$data)@data$SampleID) %>%
          dplyr::ungroup() %>%
          dplyr::select(SampleID, Num_peaks = n)
      },
      striped = TRUE,
      bordered = TRUE,
      width = '100%'
      )
    }) %>%
      bindEvent(input$pick)

    output$has_peaks <- renderUI({
      if(has_peaks()){
        colored_text('Peak picking finished successfully.', color = 'green')
      } else {
        colored_text('Error with Peak picking', color = 'red')
      }
    }) %>%
      bindEvent(input$pick)

    ## Button to move to next step ----

    output$next_buttonPP <- renderUI({

      if(has_peaks()){
        next_button(id = 'next_buttonPP')
      }
    }) %>%
      bindEvent(input$pick)

  })
}

## To be copied in the UI
# mod_peak_picking_ui("peak_picking_1")

## To be copied in the server
# mod_peak_picking_server("peak_picking_1")
