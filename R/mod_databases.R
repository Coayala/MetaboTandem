#' databases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_databases_ui <- function(id) {
  ns <- NS(id)
  tagList(

    dashboardPage(
      app_header(home = FALSE),

      dashboardSidebar(
        fluidRow(
          column(6, align = 'center', offset = 3,
                 br(),
                 back_button(id = 'goHome_databases')
          )
        )
      ),

      dashboardBody(

        h1('Manage Databases'),

        headerbox_factory(
          title = 'MassBank',
          status = 'primary',
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          content = tagList(
            p('The MassBank database downloads itself everytime it is used, so there is no need to prepare it
              before hand')
          )
        ),

        headerbox_factory(
          title = 'MassBank of North America (MoNA)',
          status = 'primary',
          width = 12,
          id = ns('mona_box'),
          collapsible = TRUE,
          collapsed = TRUE,
          content = tagList(
            fluidRow(
              col_6(
                fileInput(ns('mona_structure_file'), 'MoNA structure file (SDF)', accept = c('.sdf')),
              ),
              col_6(
                shinyWidgets::airDatepickerInput(ns('mona_date'),
                                                 label = "Date",
                                                 value = format(Sys.time(), "%Y-%m-%d"),
                                                 maxDate = format(Sys.time(), "%Y-%m-%d"),
                                                 minDate = "2001-01-01",
                                                 view = "months",
                                                 minView = "months",
                                                 dateFormat = "yyyy-MM"),
                textInput(ns('mona_version'), 'Version')

              )
            ),
            fluidRow(
              col_6(),
              col_6(
                shinyWidgets::actionBttn(
                  inputId = ns('build_mona'),
                  label = 'Build Database',
                  style = 'jelly',
                  color = 'primary',
                  size = 'sm',
                  block = TRUE)
              )
            ),
            fluidRow(
              uiOutput(ns('mona_res'))
            )
          )
        ),

        headerbox_factory(
          title = 'Human Metabolome Database',
          status = 'primary',
          width = 12,
          id = ns('hmdb_box'),
          collapsible = TRUE,
          collapsed = TRUE,
          content = tagList(
            fluidRow(
              col_6(
                fileInput(ns('hmdb_structure_file'), 'HMDB structure file (SDF)', accept = c('.sdf')),
                shinyDirButton(ns('hmdb_spectra_dir'), 'Folder with spectra files',
                               'Please select folder with spectra files',
                               FALSE),
                br(), br(),
                uiOutput(ns('hmdb_sel_dir'))
              ),
              col_6(
                shinyWidgets::airDatepickerInput(ns('hmdb_date'),
                                                 label = "Date",
                                                 value = format(Sys.time(), "%Y-%m-%d"),
                                                 maxDate = format(Sys.time(), "%Y-%m-%d"),
                                                 minDate = "2001-01-01",
                                                 view = "months",
                                                 minView = "months",
                                                 dateFormat = "yyyy-MM"),
                textInput(ns('hmdb_version'), 'Version'),
                checkboxInput(ns('hmdb_predicted'), 'Using predicted dataset?', value = FALSE)

              )
            ),
            fluidRow(
              col_6(),
              col_6(
                shinyWidgets::actionBttn(
                  inputId = ns('build_hmdb'),
                  label = 'Build Database',
                  style = 'jelly',
                  color = 'primary',
                  size = 'sm',
                  block = TRUE)
              )
            ),
            fluidRow(
              uiOutput(ns('hmdb_res'))
            )
          )
        ),

        headerbox_factory(
          title = 'Global Natural Product Social Molecular Networking (GNPS)',
          status = 'primary',
          width = 12,
          id = ns('gnps_box'),
          collapsible = TRUE,
          collapsed = TRUE,
          content = tagList(
            fluidRow(
              col_6(
                fileInput(ns('gnps_compounds_file'), 'GNPS compounds file (CSV)', accept = c('.csv')),
                fileInput(ns('gnps_spectra_file'), 'GNPS spectra file (MGF)', accept = c('.mgf')),
              ),
              col_6(
                shinyWidgets::airDatepickerInput(ns('gnps_date'),
                                                 label = "Date",
                                                 value = format(Sys.time(), "%Y-%m-%d"),
                                                 maxDate = format(Sys.time(), "%Y-%m-%d"),
                                                 minDate = "2001-01-01",
                                                 view = "months",
                                                 minView = "months",
                                                 dateFormat = "yyyy-MM"),
                textInput(ns('gnps_version'), 'Version')

              )
            ),
            fluidRow(
              col_6(),
              col_6(
                shinyWidgets::actionBttn(
                  inputId = ns('build_gnps'),
                  label = 'Build Database',
                  style = 'jelly',
                  color = 'primary',
                  size = 'sm',
                  block = TRUE)
              )
            ),
            fluidRow(
              uiOutput(ns('gnps_res'))
            )
          )
        )


      )

    )
  )
}

#' databases Server Functions
#'
#' @noRd
mod_databases_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    shinyDirChoose(input, 'hmdb_spectra_dir', roots = c('wd' = getwd(), 'root' = '/'),
                   session = session, defaultRoot = 'wd')

    output$hmdb_sel_dir <- renderUI({
      tagList(
        fluidRow(
          col_12(verbatimTextOutput(ns('hmdb_dir_path')))
        )
      )
    }) %>%
      bindEvent(input$hmdb_spectra_dir)

    output$hmdb_dir_path <- renderText(
      paste(parseDirPath(roots = c('wd' = getwd(), 'root' = '/'), input$hmdb_spectra_dir))
    ) %>%
      bindEvent(input$hmdb_spectra_dir)


    output$mona_res <- renderUI({

      create_mona_db(structure_file = input$mona_structure_file$datapath,
                     source_version = input$mona_version,
                     source_date = input$mona_date)

      file_name <- paste0('CompDb.MoNA', input$mona_version, '.sqlite')

      if(file.exists(file_name)){
        tagList(
          hr(),
          colored_text('MoNA database build correctly', color = 'green')
        )
      } else {
        tagList(
          hr(),
          colored_text('MoNA database failed building', color = 'red')
        )
      }

    }) %>%
      bindEvent(input$build_mona)

    output$hmdb_res <- renderUI({

      create_hmdb_db(structure_file = input$hmdb_structure_file$datapath,
                     spectra_directory = parseDirPath(roots = c('wd' = getwd(), 'root' = '/'),
                                                      input$hmdb_spectra_dir),
                     source_version = input$hmdb_version,
                     source_date = input$hmdb_date,
                     predicted = input$hmdb_predicted)

      file_name <- paste0('CompDb.HMDB', input$hmdb_version, '.sqlite')

      if(file.exists(file_name)){
        tagList(
          hr(),
          colored_text('HMDB database build correctly', color = 'green')
        )
      } else {
        tagList(
          hr(),
          colored_text('HMDB database failed building', color = 'red')
        )
      }

    }) %>%
      bindEvent(input$build_hmdb)

    output$gnps_res <- renderUI({

      create_gnps_db(compounds_info_file = input$gnps_compounds_file$datapath,
                     spectra_mgf = input$gnps_spectra_file$datapath,
                     source_version = input$gnps_version,
                     source_date = input$gnps_date)

      file_name <- paste0('CompDb.GNPS', input$gnps_version, '.sqlite')

      if(file.exists(file_name)){
        tagList(
          hr(),
          colored_text('GNPS database build correctly', color = 'green')
        )
      } else {
        tagList(
          hr(),
          colored_text('GNPS database failed building', color = 'red')
        )
      }

    }) %>%
      bindEvent(input$build_gnps)


  })
}

## To be copied in the UI
# mod_databases_ui("databases_1")

## To be copied in the server
# mod_databases_server("databases_1")
