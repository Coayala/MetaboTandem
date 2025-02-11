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
        checkboxInput(ns('use_gap'), 'Apply gap filling?', value = FALSE),
        uiOutput(ns('gap_params'))
      )
    ),

    # Space for gap filling results


    # Buttons to move along the wizard

    fluidRow(
      col_12(uiOutput(ns('gap_results'))),
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

    # Waiter
    waiter_gf <- waiter::Waiter$new(id = ns('gf_box'),
                                    html = waiter::spin_hexdots(),
                                    color = waiter::transparent(.1))

    # Parameters for gap filling
    output$gap_params <- renderUI({
      if(input$use_gap){
        col_12(
          numericInput(ns('cores'), "Number of Cores", value = 1,
                       min = 1, max = parallel::detectCores()),
          shinyWidgets::actionBttn(
            inputId = ns('fill'),
            label = 'Apply gap filling',
            style = 'jelly',
            color = 'primary',
            size = 'sm',
            block = TRUE
          )
        )
      }
    })

    output$gap_results <- renderUI({
      headerbox_factory(
        title = 'Gap filling results',
        status = 'success',
        width = 12,
        content = tagList(
          col_12(tableOutput(ns('filled_peaks')),
                 hr(),
                 plotOutput(ns('gap_plot')),
                 uiOutput(ns('is_filled')))
        )
      )
    }) %>%
      bindEvent(input$fill)

    has_filled <- reactive({
      if(input$use_gap){
        req(input$fill)
        waiter_gf$show()

        MTandem_obj$apply_gap_filling(cores = input$cores)

        waiter_gf$hide()

        xcms::hasFilledChromPeaks(MTandem_obj$data)
      } else {
        FALSE
      }

    })

    output$filled_peaks <- renderTable({
      if(has_filled()){
        before <- xcms::featureValues(MTandem_obj$data, filled = FALSE)
        after <- xcms::featureValues(MTandem_obj$data, filled = TRUE)


        gf_df <- tidyr::tibble(SampleID = colnames(before),
                               bef = colSums(!is.na(before)),
                               af = colSums(!is.na(after))) %>%
          dplyr::mutate(diff = af - bef,
                        detect = af - diff) %>%
          dplyr::rename(`Detected peaks` = detect,
                        `Filled peaks` = diff,
                        Total = af)
      }
    },
    striped = TRUE,
    bordered = TRUE,
    width = '100%'
    )  %>%
      bindEvent(input$fill)


    output$gap_plot <- renderPlot({
      if(has_filled()){
        before <- xcms::featureValues(MTandem_obj$data, filled = FALSE)
        after <- xcms::featureValues(MTandem_obj$data, filled = TRUE)


        gf_df <- tidyr::tibble(SampleID = colnames(before),
                               bef = colSums(!is.na(before)),
                               af = colSums(!is.na(after))) %>%
          dplyr::mutate(diff = af - bef,
                        af = af - diff) %>%
          tidyr::pivot_longer(!SampleID, names_to = 'type', values_to = 'value') %>%
          dplyr::mutate(proc = ifelse(type == 'bef', 'Before Gap Filling', 'After Gap Filling'),
                        type = ifelse(type == 'bef' | type == 'af', 'Detected Peaks', 'Filled Peaks'),
                        proc = factor(proc, levels = c('Before Gap Filling', 'After Gap Filling')),
                        type = factor(type, levels = c('Filled Peaks', 'Detected Peaks')))

        gf_plot <- gf_df %>%
          ggplot2::ggplot(ggplot2::aes(x = SampleID,
                                       y = value,
                                       fill = type)) +
          ggplot2::geom_col(color = 'black') +
          ggplot2::labs(y = 'Number of peaks') +
          ggplot2::facet_wrap(~proc, nrow = 2) +
          ggplot2::scale_fill_manual(values = c('Detected Peaks' = 'steelblue',
                                                'Filled Peaks' = 'orange2')) +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.grid = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_text(angle = 45,
                                                             hjust = 1),
                         legend.title = ggplot2::element_blank(),
                         legend.position = 'bottom')

        gf_plot
      }
    }) %>%
      bindEvent(input$fill)

    output$is_filled <- renderUI({
      if(has_filled()){
        res <- colored_text('Gap filling completed successfully.',
                            color = 'green')

      } else {
        res <- colored_text('No features were detected', color = 'red')
      }

      res
    })

    output$next_buttonsGF <- renderUI({
      if(!input$use_gap || has_filled()){
        tagList(
          other_arrow_button(id = 'GF_stats',
                             label = 'Go to Statistical Analysis'),
          br(),
          other_arrow_button(id = 'GF_annot',
                             label = 'Go to Annotation')
        )
      }
    })



  })
}

## To be copied in the UI
# mod_gap_filling_ui("gap_filling_1")

## To be copied in the server
# mod_gap_filling_server("gap_filling_1")
