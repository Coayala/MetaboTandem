#' annotation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annotation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    headerbox_factory(
      title = 'Annotation Parameters',
      status = 'primary',
      width = 12,
      id = ns('spec_annot_box'),
      content = tagList(
        fluidRow(
          col_12(uiOutput(ns('if_solo'))),
          col_6(shinyWidgets::virtualSelectInput(
            ns('annot_db'),
            'Annotation databases',
            choices = c(
              'Massbank' = 'massbank',
              'MoNA' = 'mona',
              'HMDB (experimental)' = 'hmdb_exp',
              'HMDB (predicted)' = 'hmdb_pred',
              'GNPS' = 'gnps'
            ),
            multiple = TRUE,
            showValueAsTags = TRUE
          )),
          col_6(checkboxInput(ns('req_prec'), 'Require precursor m/z',
                              value = TRUE))
        ),
        fluidRow(
          col_6(shinyWidgets::virtualSelectInput(
            ns('adducts'),
            'Adducts',
            choices = list(
              Positive = MetaboCoreUtils::adducts('positive')$name,
              Negative = MetaboCoreUtils::adducts('negative')$name),
            multiple = TRUE
          ),
          numericInput(ns('ppm'), 'PPM', value = 5, step = 0.1),
          numericInput(ns('candidates'), 'Candidate annotations', value = 1,
                       step = 1)),
          col_6(numericInput(ns('tolerance'), 'm/z Tolerance', value = 0.005),
                numericInput(ns('dist_thres'), 'Spectra distance threshold',
                             value = 0.5, step = 0.1, min = 0, max = 1))
        ),
        fluidRow(
          col_12(hr()),
          col_12(h3(strong('Molecular Classification')))
        ),
        fluidRow(
          col_6(
            checkboxInput(ns('use_classyfire'), 'Use Classyfire',
                          value = FALSE)
          )
        ),
        fluidRow(
          col_6(
            checkboxInput(ns('use_sirius'), 'Use CANOPUS (part of the SIRIUS suite)',
                          value = FALSE)),
          col_6(uiOutput(ns('sirius_params')))
        ),
        fluidRow(
          col_6(),
          col_6(
            shinyWidgets::actionBttn(
              inputId = ns('annotate'),
              label = 'Annotate metabolites',
              style = 'jelly',
              color = 'primary',
              size = 'sm',
              block = TRUE
            )
          )
        )
      )
    ),

    fluidRow(
      col_12(uiOutput(ns('annotate_results'))),

      # Buttons to move along the wizard
      col_3(back_button(id = 'back_buttonAN')),
      col_3(),
      col_6(uiOutput(ns('next_buttonsAN')))
    )
  )
}

#' annotation Server Functions
#'
#' @noRd
mod_annotation_server <- function(id, MTandem_obj, solo = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # Waiter
    waiter_ann <- waiter::Waiter$new(id = c(ns('spec_annot_box')),
                                     html = waiter::spin_hexdots(),
                                     color = waiter::transparent(.1))

    # Logic for solo app
    if(solo){
      output$if_solo <- renderUI({
        fluidRow(
          col_6(fileInput(ns('mgf_file'), 'Spectra file', accept = c('.mgf')),
                uiOutput(ns('mgf_loaded'))),
          col_6(fileInput(ns('feature_def_file'), 'Feature definitions file', accept = c('.csv', '.tsv')),
                uiOutput(ns('check_feature_def')))
        )

      })
    }

    output$mgf_loaded <- renderUI({

      MTandem_obj$feature_spectra <- load_mgf_spectra(input$mgf_file$datapath)

      if(is(MTandem_obj$feature_spectra, 'Spectra')){
        colored_text('Spectra loaded correctly', color = 'green')
      } else {
        colored_text('Please load spectra in mgf format', color = 'red')
      }

    }) %>%
      bindEvent(input$mgf_file)

    output$check_feature_def <- renderUI({
      MTandem_obj$feature_definitions <- as.data.frame(load_dataframe(input$feature_def_file$datapath))

      if(!is.null(MTandem_obj$feature_definitions)){
        colored_text('Feature definitions loaded correctly', color = 'green')
      }
    }) %>%
      bindEvent(input$feature_def_file)

    output$sirius_params <- renderUI({
      if(input$use_sirius){
        tagList(
          textInput(ns('sirius_out_prefix'), 'SIRIUS results output prefix'),
          numericInput(ns('sirius_cores'), 'Num. cores', value = 1,
                       min = 1, max = parallel::detectCores())
        )
      }
    })


    # Logic for annotation
    has_annot <- reactive({
      waiter_ann$show()
      MTandem_obj$get_annotation_tables(selected_dbs = input$annot_db,
                                        adducts = input$adducts,
                                        tolerance = input$tolerance,
                                        ppm = input$ppm,
                                        req_precursor = input$req_prec,
                                        distance_thres = input$dist_thres,
                                        candidates = input$candidates)

      MTandem_obj$merge_annotation_tables(candidates = input$candidates)

      if(input$use_classyfire){
        MTandem_obj$run_classyfire()
      }

      if(input$use_sirius){
        MTandem_obj$run_sirius(input$sirius_out_prefix,
                               input$sirius_cores)
      }

      if(input$use_classyfire | input$use_sirius){
        MTandem_obj$add_molecular_classes()
      }

      waiter_ann$hide()

      !is.null(MTandem_obj$annotation_merged)
    }) %>%
      bindEvent(input$annotate)

    output$annotate_results <- renderUI({
      req(has_annot())

      group_var <- MTandem_obj$get_groups()

      if(solo){
        headerbox_factory(
          title = 'Annotation Results',
          status = 'success',
          width = 12,
          content = tagList(
            fluidRow(
              col_12(plotOutput(ns('mirror_plot')))
            ),
            col_12(DT::DTOutput(ns('merged_annot')))

          )
        )
      } else {
        headerbox_factory(
          title = 'Annotation Results',
          status = 'success',
          width = 12,
          content = tagList(
            fluidRow(
              col_6(selectInput(ns('color_by'), 'Color by', choices = group_var))
            ),
            fluidRow(
              col_6(plotOutput(ns('spectra_plot'))),
              col_6(plotOutput(ns('mirror_plot')))
            ),
            col_12(DT::DTOutput(ns('merged_annot')))

          )
        )
      }
    })

    output$merged_annot <- DT::renderDT({

      if(has_annot()){
        MTandem_obj$annotation_merged %>%
          dplyr::mutate(mz = round(mz, 4),
                        rtime = round(rtime, 2),
                        target_precursorMz = round(target_precursorMz, 4),
                        target_exactmass = round(target_exactmass, 4),
                        ms1_ppm_error = round(ms1_ppm_error, 4),
                        ms1_score = format(ms1_score, scientific = TRUE, digits = 2),
                        ms2_score = round(ms2_score, 2))


      }
    },
    fillContainer = TRUE,
    selection = 'single',
    extensions = c('FixedColumns'),
    rownames = FALSE,
    options = list(scrollY = "300px",
                   scrollX = TRUE,
                   dom = 'tpB',
                   fixedColumns = list(leftColumns = 1),
                   lineHeight = "50%")) %>%
      bindEvent(input$annotate)

    output$spectra_plot <- renderPlot({
      req(has_annot())
      if(!is.null(input$merged_annot_rows_selected)){
        s = input$merged_annot_rows_selected
      } else {
        s = 1
      }

      group_colors <- setNames(
        ggpubr::get_palette(
          palette = 'Dark2',
          length(unique(MTandem_obj$data@sampleData[[input$color_by]]))
        ),
        nm = unique(MTandem_obj$data@sampleData[[input$color_by]])
      )

      color_vector <- group_colors[MTandem_obj$data@sampleData[[input$color_by]]]

      xcms::plot(MTandem_obj$feature_chromatograms[s,],
                 col = color_vector)
      legend('topleft', legend = names(group_colors), col = group_colors,
             lty = rep(1, length(group_colors)), cex = .8,
             lwd = rep(2, length(group_colors)))
    }) %>%
      bindEvent(input$merged_annot_rows_selected)

    output$mirror_plot <- renderPlot({
      req(has_annot())
      if(!is.null(input$merged_annot_rows_selected)){
        s = input$merged_annot_rows_selected
      } else {
        s = 1
      }

      has_ms2_annot <- as.logical(
        MTandem_obj$annotation_merged$annotation_from_MS2[s]
      )

      if(has_ms2_annot){
        feature_picked <- MTandem_obj$annotation_merged$feature_id[s]

        MTandem_obj$plot_mirror(feature_picked)
      } else {
        plot.new()
      }

    })


  })
}

## To be copied in the UI
# mod_annotation_ui("annotation_1")

## To be copied in the server
# mod_annotation_server("annotation_1")
