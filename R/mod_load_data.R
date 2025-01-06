#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Load metadata
    headerbox_factory(
      title = 'Load Metadata',
      status = 'primary',
      width = 6,
      content = tagList(
        fileInput(ns('metadata_file'), 'Choose file', accept = c('.csv', '.tsv')),
        DT::DTOutput(ns('metadata_table'))
      )
    ),

    # Load spectra
    uiOutput(ns('load_spectra'))
  )
}

#' load_data Server Functions
#'
#' @noRd
mod_load_data_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Load metadata server functions ----

    w_spectra_load <- waiter::Waiter$new(id = ns('spectra_box'),
                                         html = waiter::spin_hexdots(),
                                         color = waiter::transparent(.1))

    output$metadata_table <- DT::renderDT(
      {
        MTandem_obj$load_metadata(input$metadata_file$datapath)
        MTandem_obj$metadata
      },
      options = list(scrollX = TRUE,
                     dom = 'ltip')
    ) %>%
      bindEvent(input$metadata_file)


    # Load spectra UI ----
    output$load_spectra <- renderUI({
      headerbox_factory(
        title = 'Load Spectra Data',
        status = 'primary',
        width = 6,
        id = ns('spectra_box'),
        content = tagList(
          shinyDirButton(ns('datadir'), 'Choose data folder',
                         'Please select folder with data',
                         FALSE),
          br(), br(),
          htmlOutput(ns('sel_directory')),
          br(),
          selectInput(ns('format'), 'Data format', c('mzML', 'mzXML')),
          htmlOutput(ns('is_loaded')),
          br(),
          fluidRow(
            column(6,
                   shinyWidgets::actionBttn(ns('load'), 'Load Data',
                                            style = 'jelly',
                                            color = 'primary',
                                            block = TRUE,
                                            size = 'sm')),
            column(6,
                   uiOutput(ns('nxt_bttn')))
          )
        )
      )
    }) %>%
      bindEvent(input$metadata_file)


    # Get directory for files ----
    shinyDirChoose(input, 'datadir', roots = c('wd' = '.'), session = session)

    # Print selected directory ----
    output$sel_directory <- renderText(
      paste(strong('Directory selected:'), '<br/>', parseDirPath(roots = c('wd' = '.'), input$datadir))
    ) %>%
      bindEvent(input$datadir)

    # Load spectra data and transform in centroid ----

    output$is_loaded <- renderText({
      w_spectra_load$show()
      notid <- showNotification('Reading data...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      MTandem_obj$load_spectra_data(datadir = parseDirPath(roots = c('wd' = '.'), input$datadir),
                                    format = input$format)

      MTandem_obj$centroid_check()
      w_spectra_load$hide()
      if(is(MTandem_obj$data, 'OnDiskMSnExp')){
        as.character(
          colored_text('Data loaded correctly', color = 'green')
        )
      } else {
        as.character(
          colored_text('Error. Check data directory and load data again', color = 'red')
        )
      }

    }) %>%
      bindEvent(input$load)

    # Button to move to next step
    output$nxt_bttn <- renderUI({
      if(is(MTandem_obj$data, 'OnDiskMSnExp')){
        next_button(id = 'next_buttonLD')
      }
    }) %>%
      bindEvent(input$load)
  })


}

## To be copied in the UI
# mod_load_data_ui("load_data_1")

## To be copied in the server
# mod_load_data_server("load_data_1")
