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
    # Load metadata box
    fluidRow(
      headerbox_factory(
        title = 'Load Metadata',
        status = 'primary',
        width = 6,
        content = tagList(
          fileInput(ns('metadata_file'), 'Choose file', accept = c('.csv', '.tsv'))
        )
      ),
      # Load spectra box
      uiOutput(ns('load_spectra'))

    ),

    # Result boxes
    fluidRow(
      uiOutput(ns('metadata_results')),
      uiOutput(ns('spectra_results'))
    ),

    # next_button

    fluidRow(
      col_9(),
      col_3(uiOutput(ns('nxt_bttn')))
    )
  )
}

#' load_data Server Functions
#'
#' @noRd
mod_load_data_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Waiter
    w_spectra_load <- waiter::Waiter$new(id = ns('spectra_box'),
                                         html = waiter::spin_hexdots(),
                                         color = waiter::transparent(.1))

    output$metadata_results <- renderUI({
      headerbox_factory(title = 'Metadata table',
                        status = 'success',
                        width = 6,
                        content = tagList(
                          col_12(DT::DTOutput(ns('metadata_table')))
                        ))
    }) %>%
      bindEvent(input$metadata_file)

    # Load metadata server functions ----
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
          uiOutput(ns('other_spec_params'))
        )
      )
    }) %>%
      bindEvent(input$metadata_file)

    # Get directory for files ----
    shinyDirChoose(input, 'datadir', roots = c('wd' = '.'), session = session)

    output$other_spec_params <- renderUI({
      tagList(
        fluidRow(
          col_12(htmlOutput(ns('sel_directory'))),
          col_12(selectInput(ns('format'), 'Data format', c('mzML', 'mzXML')))
        ),
        fluidRow(
          column(6,
                 shinyWidgets::actionBttn(ns('load'), 'Load Data',
                                          style = 'jelly',
                                          color = 'primary',
                                          block = TRUE,
                                          size = 'sm'))
        )
      )
    }) %>%
      bindEvent(input$datadir)

    # Print selected directory ----
    output$sel_directory <- renderText(
      paste(strong('Directory selected:'), '<br/>', parseDirPath(roots = c('wd' = '.'), input$datadir))
    ) %>%
      bindEvent(input$datadir)

    # Load spectra data and transform in centroid ----

    is_loaded <- reactive({
      w_spectra_load$show()
      notid <- showNotification('Reading data...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      MTandem_obj$load_spectra_data(datadir = parseDirPath(roots = c('wd' = '.'), input$datadir),
                                    format = input$format)

      MTandem_obj$centroid_check()
      w_spectra_load$hide()

      if(is(MTandem_obj$data, 'OnDiskMSnExp')){
        colored_text('Data loaded correctly', color = 'green')

      } else {
        colored_text('Error. Check data directory and load data again', color = 'red')

      }
    })

    # Showing results
    observe({
      req(is_loaded())
      output$spectra_results <- renderUI({
        headerbox_factory(title = 'Spectra Overview',
                          status = 'success',
                          width = 6,
                          content = tagList(
                            tableOutput(ns('spectra_table')),
                            uiOutput(ns('is_loaded'))
                          ))
      })
    }) %>%
      bindEvent(input$load)

    # Check if data is loaded
    output$is_loaded <- renderUI({
      is_loaded()
    }) %>%
      bindEvent(input$load)

    # Check how many spectra
    observe({
      req(is_loaded())
      output$spectra_table <- renderTable({
        table(xcms::fromFile(MTandem_obj$data)) %>%
          as.data.frame() %>%
          dplyr::mutate(SampleID = xcms::phenoData(MTandem_obj$data)@data$SampleID) %>%
          dplyr::select(SampleID, `Num. spectra` = Freq)
      },
      striped = TRUE,
      bordered = TRUE,
      width = '100%')
    }) %>%
      bindEvent(input$load)

    # Button to move to next step
    observe({
      req(is_loaded())
      output$nxt_bttn <- renderUI({
        if(is(MTandem_obj$data, 'OnDiskMSnExp')){
          next_button(id = 'next_buttonLD')
        }
      })
    }) %>%
      bindEvent(input$load)
  })
}

## To be copied in the UI
# mod_load_data_ui("load_data_1")

## To be copied in the server
# mod_load_data_server("load_data_1")
