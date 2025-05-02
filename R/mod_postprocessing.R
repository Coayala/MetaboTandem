#' gap_filling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_postprocessing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    headerbox_factory(
      title = 'Post processing options',
      status = 'primary',
      width = 12,
      id = ns('post_proc_box'),
      content = tagList(
        fluidRow(
          col_6(checkboxInput(ns('use_gap'),
                              'Apply gap filling?', value = FALSE)),
          col_6(uiOutput(ns('gap_params')))
        ),
        fluidRow(
          col_6(uiOutput(ns('do_clean'))),
          col_6(uiOutput(ns('clean_params')))
        ),
        fluidRow(
          col_6(),
          col_6(uiOutput(ns('apply_button')))
        )
      )
    ),

    # Space for gap filling results

    # MetaClean results

    # Buttons to move along the wizard

    fluidRow(
      col_12(uiOutput(ns('gap_results'))),
      col_3(back_button(id = 'back_buttonPOST')),
      col_3(),
      col_6(uiOutput(ns('next_buttonsPOST')))
    )

  )
}

#' gap_filling Server Functions
#'
#' @noRd
mod_postprocessing_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Waiter
    waiter_gf <- waiter::Waiter$new(id = ns('post_proc_box'),
                                    html = waiter::spin_hexdots(),
                                    color = waiter::transparent(.1))

    # Parameters for gap filling
    output$gap_params <- renderUI({
      if(input$use_gap){
        numericInput(ns('cores'), "Number of Cores", value = 1,
                     min = 1, max = parallel::detectCores())
      }
    })

    output$do_clean <- renderUI({
      if(input$use_gap){
        checkboxInput(ns('use_clean'),
                      'Clean badly integrated peaks?', value = FALSE)
      }
    })

    output$clean_params <- renderUI({
      req(input$use_clean)
      group_var <- MTandem_obj$get_groups()
      if(input$use_clean){
        tagList(
          selectizeInput(ns('group_by'), 'Group by', choices = group_var),
          selectizeInput(ns('model_file'), 'Select model',
                         choices = c('RP model - soil data' = 'test_data/metaclean_model.rds',
                                     'Choice your own' = 'choice'),
                         selected = 'test_data/metaclean_model.rds'),
          uiOutput(ns('choice'))
        )
      }
    })

    output$choice <- renderUI({
      if(input$model_file == 'choice'){
        fileInput(ns('own_file'), 'Choose model file', accept = c('.rds'))
      }
    })

    output$apply_button <- renderUI({
      if(input$use_gap){
        shinyWidgets::actionBttn(
          inputId = ns('apply_post_proc'),
          label = 'Apply gap filling',
          style = 'jelly',
          color = 'primary',
          size = 'sm',
          block = TRUE
        )
      }
    })

    # Gap cleaning ----

    output$gap_results <- renderUI({
      req(has_filled())
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
    })

    has_filled <- reactive({
      if(input$use_gap){
        waiter_gf$show()
        notid <- showNotification('Applying gap filling...',
                                  duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(notid), add = TRUE)
        MTandem_obj$apply_gap_filling(cores = input$cores)

        waiter_gf$hide()

        xcms::hasFilledChromPeaks(MTandem_obj$data)
      } else {
        FALSE
      }
    }) %>%
      bindEvent(input$apply_post_proc)

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
      bindEvent(input$apply_post_proc)


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
      bindEvent(input$apply_post_proc)

    output$is_filled <- renderUI({
      if(has_filled()){
        res <- colored_text('Gap filling completed successfully.',
                            color = 'green')

      } else {
        res <- colored_text('No features were detected', color = 'red')
      }

      res
    })

    # MetaClean ----

    output$clean_results <- renderUI({
      req(clean_plot())
      headerbox_factory(
        title = 'MetaClean results',
        status = 'success',
        width = 12,
        content = tagList(
          col_12(plotOutput(ns('clean_plot')),
                 uiOutput(ns('is_clean')))
        )
      )
    })

    clean_plot <- reactive({
      if(input$use_clean){

        waiter_gf$show()
        notid <- showNotification('Cleaning peaks...',
                                  duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(notid), add = TRUE)

        init_ft <- xcms::featureDefinitions(MTandem_obj$data) %>% nrow()

        MTandem_obj$apply_cleaning(group_by = input$group_by,
                                   model_file = input$model_file)

        remaining_ft <- xcms::featureDefinitions(MTandem_obj$data) %>% nrow()

        waiter_gf$hide()

        plot <- data.frame(Features = c('Pass', 'Removed'),
                           n = c(remaining_ft, init_ft - remaining_ft)) %>%
          dplyr::mutate(perc = round(n / sum(n) * 100, 2),
                        label = paste0(perc, '%'),
                        label_y = cumsum(perc) - 0.5*perc) %>%
          ggplot2::ggplot() +
          ggplot2::geom_col(ggplot2::aes(x = 1,
                                         y = perc,
                                         fill = rev(Features)),
                            color = 'black') +
          ggplot2::geom_text(ggplot2::aes(x = 1,
                                          y = label_y,
                                          label = label)) +
          ggplot2::scale_fill_manual(values = c('darkgreen', 'gray80')) +
          ggplot2::coord_polar('y') +
          ggplot2::labs(fill = 'Features') +
          ggplot2::theme_void() +
          ggplot2::theme(legend.position = 'right')

        plot

      }
    }) %>%
      bindEvent(input$apply_post_proc)

    output$clean_plot <- renderPlot({
      clean_plot()
    })

    output$is_clean <- renderUI({
      req(clean_plot())
      pass <- clean_plot()$data$n[1]
      removed <- clean_plot()$data$n[2]
      text <- paste0(removed, ' features removed. ', pass, ' features remaining')

      res <- colored_text(text,
                          color = 'green')

      res
    })

    output$next_buttonsPOST <- renderUI({
      if(!(input$use_gap) || (has_filled() | is(clean_plot(), 'ggplot'))){
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
# mod_postprocessing_ui("postprocessing_1")

## To be copied in the server
# mod_postprocessing_ui("postprocessing_1")
