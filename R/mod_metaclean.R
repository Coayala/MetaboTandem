#' metaclean UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metaclean_ui <- function(id) {
  ns <- NS(id)
  tagList(
    headerbox_factory(
      title = 'MetaClean training options',
      status = 'primary',
      width = 12,
      id = ns('metaclean_box'),
      content = tagList(
        fluidRow(
          col_6(numericInput(ns('n_subset'),
                             'Number of peaks to label',
                             value = 30,
                             min = 30,
                             max = 1000,
                             step = 10)),
          col_6((shinyWidgets::numericInputIcon(ns('prop_training'),
                                                'Proportion of peaks used for training',
                                                value = 50,
                                                min = 50,
                                                max = 90,
                                                step = 5,
                                                icon = list(NULL, icon("percent"))))
          ),
          fluidRow(
            col_6(),
            col_6(
              shinyWidgets::actionBttn(
                inputId = ns('start_mc'),
                label = 'Start MetaClean',
                style = 'jelly',
                color = 'primary',
                size = 'sm',
                block = TRUE))
          )
        )
      )
    ),

    uiOutput(ns('metaclean_res'))
  )
}

#' metaclean Server Functions
#'
#' @noRd
mod_metaclean_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$metaclean_res <- renderUI({

      headerbox_factory(
        title = 'MetaClean training visualizer',
        status = 'success',
        width = 12,
        id = ns('metaclean_resbox'),
        content = tagList(
          fluidRow(
            uiOutput(ns('chrom_loop')),
            uiOutput(ns('cross_val_res'))
          )
        )
      )

    }) %>%
      bindEvent(start_qc())

    has_filled_mc <- reactive({
      notid <- showNotification('Applying gap filling...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      MTandem_obj$apply_gap_filling(cores = parallel::detectCores())

      MTandem_obj$extract_feature_definitions()


      xcms::hasFilledChromPeaks(MTandem_obj$data)

    }) %>%
      bindEvent(input$start_mc)

    rv <- reactiveValues(
      idx = 1,
      sel_length = 0,
      phase = 'INIT',
      has_models = FALSE,
      has_final_model = FALSE
    )

    start_qc <- reactive({
      notid <- showNotification('Preparing data for cleaning...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      MTandem_obj$prepare_for_cleaning(n_subset = input$n_subset)
      MTandem_obj$labels_shiny <- data.frame(
        feature_id = MTandem_obj$feature_definitions$feature_id[MTandem_obj$prepare_obj$sel_peaks]
      ) %>%
        dplyr::mutate(Label = NA)
      rv$phase <- 'QC'
      rv$sel_length <- input$n_subset
    }) %>%
      bindEvent(has_filled_mc())

    output$chrom_loop <- renderUI({
      if(rv$idx <= rv$sel_length){
        tagList(
          col_12(plotOutput(ns('chrom_plot'))),
          col_6(shinyWidgets::actionBttn(ns('fail_btn'),
                                         'Bad Peak',
                                         style = 'jelly',
                                         color = 'danger',
                                         block = TRUE,
                                         size = 'sm',
                                         icon = icon('ban'))),
          col_6(shinyWidgets::actionBttn(ns('pass_btn'),
                                         'Good Peak',
                                         style = 'jelly',
                                         color = 'success',
                                         block = TRUE,
                                         size = 'sm',
                                         icon = icon('check')))
        )
      }
    })

    output$chrom_plot <- renderPlot({
      req(MTandem_obj$prepare_obj$chroms)
      i <- rv$idx
      ft_id <- MTandem_obj$labels_shiny$feature_id[i]
      xcms::plot(MTandem_obj$prepare_obj$chroms[i,], main = paste0(i, ". Feature: ", ft_id))
    })

    observeEvent(input$pass_btn, {
      i <- rv$idx
      MTandem_obj$labels_shiny$Label[i] <- "Pass"

      if (rv$idx < rv$sel_length) {
        rv$idx <- rv$idx + 1
      } else {

        MTandem_obj$labels$labels_train <-  MTandem_obj$labels_shiny[
          sample(1:nrow(MTandem_obj$labels_shiny), nrow(MTandem_obj$labels_shiny) * input$prop_training/100),
        ]

        MTandem_obj$labels$labels_train <- MTandem_obj$labels$labels_train %>%
          dplyr::mutate(EICNo = stringr::str_remove(feature_id, 'FT'),
                        EICNo = as.numeric(EICNo))

        MTandem_obj$labels$labels_test <-  MTandem_obj$labels_shiny %>%
          dplyr::filter(!feature_id %in% MTandem_obj$labels$labels_train$feature_id) %>%
          dplyr::mutate(EICNo = stringr::str_remove(feature_id, 'FT'),
                        EICNo = as.numeric(EICNo))

        showNotification("QC Finished!", type = "message")

        rv$phase <- 'CROSSVAL'

        print(MTandem_obj$labels)
      }
    })

    observeEvent(input$fail_btn, {
      i <- rv$idx
      MTandem_obj$labels_shiny$Label[i] <- "Fail"

      if (rv$idx < rv$sel_length) {
        rv$idx <- rv$idx + 1
      } else {

        MTandem_obj$labels$labels_train <-  MTandem_obj$labels_shiny[
          sample(1:nrow(MTandem_obj$labels_shiny), nrow(MTandem_obj$labels_shiny) * input$prop_training/100),
        ]

        MTandem_obj$labels$labels_train <- MTandem_obj$labels$labels_train %>%
          dplyr::mutate(EICNo = stringr::str_remove(feature_id, 'FT'),
                        EICNo = as.numeric(EICNo))

        MTandem_obj$labels$labels_test <-  MTandem_obj$labels_shiny %>%
          dplyr::filter(!feature_id %in% MTandem_obj$labels$labels_train$feature_id) %>%
          dplyr::mutate(EICNo = stringr::str_remove(feature_id, 'FT'),
                        EICNo = as.numeric(EICNo))

        showNotification("QC Finished!", type = "message")

        rv$phase <- 'CROSSVAL'

        print(MTandem_obj$labels)
      }
    })

    observeEvent(rv$phase, {

      req(rv$phase == "CROSSVAL")
      MTandem_obj$get_models()

      MTandem_obj$evaluate_models()

      rv$has_models <- TRUE

    }, ignoreInit = TRUE)

    output$cross_val_res <- renderUI({
      if(rv$has_models){
        mod_names <- purrr::map(MTandem_obj$model_eval$cd_plots$InterMet$rank_matrices, names) %>%
          purrr::reduce(`c`) %>%
          unique()

        tagList(
          col_12(plotOutput(ns('CD_plots'))),
          col_6(selectizeInput(ns('selected_model'), 'Selected model to train',
                               choices = mod_names)),
          col_6(shinyWidgets::actionBttn(
            inputId = ns('train_final'),
            label = 'Train model',
            style = 'jelly',
            color = 'primary',
            size = 'sm',
            block = TRUE
          )),
          col_12(uiOutput(ns('final_stats')))
        )
      }
    })

    output$CD_plots <- renderPlot({

      plot(MTandem_obj$model_eval$cd_plots$InterMet$plots[['Accuracy']] +
             labs(title = 'Accuracy') +
             MTandem_obj$model_eval$cd_plots$InterMet$plots[['Fail_FScore']] +
             labs(title = 'Fail FScore') +
             MTandem_obj$model_eval$cd_plots$InterMet$plots[['Pass_FScore']] +
             labs(title = 'Pass FScore') +
             patchwork::plot_layout(ncol = 1) &
             theme(plot.title = element_text(face = 'bold', hjust = 0.5)))

    })

    observeEvent(input$train_final, {

      print('Training final model')
      print(input$selected_model)
      print(names(MTandem_obj$models$models))
      MTandem_obj$train_final_model(selected_model = input$selected_model)

      rv$has_final_model <- TRUE

    }, ignoreInit = TRUE)


    output$final_stats <- renderUI({
      if(rv$has_final_model){
        tagList(
          hr(),
          h2('Final Model Test results'),
          col_12(tableOutput(ns('final_model_stats'))),
          col_6(shinyWidgets::downloadBttn(ns('down_final_model'),
                                           'Save MetaClean model (.rds)',
                                           style = 'jelly',
                                           color = 'warning',
                                           block = TRUE,
                                           size = 'xs',
                                           icon = icon('file-export')))

        )
      }

    })

    output$final_model_stats <- renderTable({
      req(MTandem_obj$final_mc_model)
      test_metrics <- MTandem_obj$test_final_model()
      test_metrics$eval_metrics

    })

    output$down_final_model <- downloadHandler(
      filename = function(){
        paste('metaclean_model.rds')
      },
      content = function(file){
        readr::write_rds(MTandem_obj$final_mc_model, file)
      }
    )

  })
}

## To be copied in the UI
# mod_metaclean_ui("metaclean_1")

## To be copied in the server
# mod_metaclean_server("metaclean_1")
