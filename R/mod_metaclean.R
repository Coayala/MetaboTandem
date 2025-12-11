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
                             value = 200,
                             min = 100,
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
            col_6(shinyWidgets::actionBttn(
              inputId = ns('start_mc'),
              label = 'Start MetaClean',
              style = 'jelly',
              color = 'primary',
              size = 'sm',
              block = TRUE
            ))
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
            uiOutput(ns('chrom_loop'))
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
      sel_peaks = NULL,
      chroms = NULL,
      xcms_fill = NULL,
      xcms_set = NULL,
      labels = NULL,
      idx = 1,
    )

    start_qc <- reactive({
      rv$idx <- 1
      ft_def <- xcms::featureDefinitions(MTandem_obj$data)

      xcms_set <- as(xcms::filterMsLevel(MTandem_obj$data, msLevel = 1), 'xcmsSet')
      xcms::sampclass(xcms_set) <- as.factor(MTandem_obj$metadata[[2]])

      rv$xcms_set <- xcms_set

      rv$xcms_fill <- xcms::fillPeaks(xcms_set)

      peak_group_names_available <- data.frame(
        FeatureID = rownames(ft_def),
        group_name = xcms::groupnames(xcms_set)
      ) %>%
        dplyr::filter(group_name %in% xcms::groupnames(rv$xcms_fill))

      rv$sel_peaks <- sample(peak_group_names_available$FeatureID, input$n_subset) %>%
        stringr::str_remove('FT') %>%
        as.numeric

      rv$chroms <- xcms::featureChromatograms(MTandem_obj$data,
                                              features = MTandem_obj$feature_definitions$feature_id[rv$sel_peaks],
                                              filled = TRUE)

      rv$labels <- data.frame(
        feature_id = MTandem_obj$feature_definitions$feature_id[rv$sel_peaks]
      ) %>%
        dplyr::mutate(Label = NA)

      return(!is.null(rv$chroms))
    }) %>%
      bindEvent(has_filled_mc())

    output$chrom_loop <- renderUI({
      if(rv$idx < length(rv$sel_peaks)){
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
                                         icon = icon('check'))),
          col_12(tableOutput(ns('labels')))
        )
      }
    }) %>%
      bindEvent(start_qc())

    output$chrom_plot <- renderPlot({
      req(rv$chroms)
      i <- rv$idx
      ft_id <- rv$labels$feature_id[i]
      xcms::plot(rv$chroms[i,], main = paste0("Feature: ", ft_id))
    })

    observeEvent(input$pass_btn, {
      i <- rv$idx
      rv$labels$Label[i] <- "Pass"

      if (rv$idx < length(rv$sel_peaks)) {
        rv$idx <- rv$idx + 1
      } else {
        showNotification("QC Finished!", type = "message")
      }
    })

    observeEvent(input$fail_btn, {
      i <- rv$idx
      rv$labels$Label[i] <- "Fail"

      if (rv$idx < length(rv$sel_peaks)) {
        rv$idx <- rv$idx + 1
      } else {
        showNotification("QC Finished!", type = "message")
      }
    })

  })
}

## To be copied in the UI
# mod_metaclean_ui("metaclean_1")

## To be copied in the server
# mod_metaclean_server("metaclean_1")
