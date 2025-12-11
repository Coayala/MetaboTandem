#' stats_univ UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_univ_ui <- function(id, solo = FALSE) {
  ns <- NS(id)
  tagList(
    # Parameter box for differential analysis ----
    headerbox_factory('Differential Analysis',
                      status = 'primary',
                      width = 12,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      id = ns('da_box'),
                      content = tagList(
                        fluidRow(
                          col_4(uiOutput(ns('opt_group'))),
                          col_4(uiOutput(ns('opt_ctr'))),
                          col_4(uiOutput(ns('opt_trt')))
                        ),
                        fluidRow(
                          col_6(),
                          col_6(shinyWidgets::actionBttn(ns('calculate'),
                                                         label = 'Calculate',
                                                         style = 'jelly',
                                                         color = 'primary',
                                                         size = 'sm',
                                                         block = TRUE))
                        )
                      )),

    uiOutput(ns('diff_results')),

    # Parameter box for model fitting ----
    headerbox_factory('Model Fitting',
                      status = 'primary',
                      width = 12,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      id = ns('model_box'),
                      content = tagList(
                        fluidRow(
                          col_6(selectizeInput(ns('model_type'),
                                               'Model',
                                               choices = c('Linear Model' = 'lm',
                                                           'Linear Mixed Effects Model' = 'lme')))
                        ),
                        fluidRow(
                          uiOutput(ns('model_params'))
                        ),
                        fluidRow(
                          col_6(),
                          col_6(shinyWidgets::actionBttn(ns('set_contrasts'),
                                                         label = 'Set contrasts',
                                                         style = 'jelly',
                                                         color = 'warning',
                                                         size = 'sm',
                                                         block = TRUE))
                        ),
                        uiOutput(ns('bottom_params'))
                      )),

    uiOutput(ns('model_results')),

    if(solo){
      fluidRow(
        col_3(back_button(id = 'back_buttonSU_solo')),
        col_3(),
        col_6(
          other_arrow_button(id = 'SU_multi_solo',
                             label = 'Go to Multivariate Analysis')
        )
      )
    } else {
      fluidRow(
        col_3(back_button(id = 'back_buttonSU')),
        col_3(),
        col_6(
          other_arrow_button(id = 'SU_multi',
                             label = 'Go to Multivariate Analysis')
        )
      )
    }
  )
}

#' stats_univ Server Functions
#'
#' @noRd
mod_stats_univ_server <- function(id, MTandem_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Waiter
    waiter_su <- waiter::Waiter$new(id = c(ns('da_box')),
                                    html = waiter::spin_hexdots(),
                                    color = waiter::transparent(.1))

    waiter_md <- waiter::Waiter$new(id = c(ns('model_box')),
                                    html = waiter::spin_hexdots(),
                                    color = waiter::transparent(.1))

    # DA server ----
    ## Group variables for differential analysis ----
    output$opt_group <- renderUI({
      group_var <- MTandem_obj$get_groups()

      tagList(
        selectizeInput(ns('group'),
                       'Experimental treatment',
                       choices = group_var)
      )
    })

    output$opt_ctr <- renderUI({

      conds <- unique(MTandem_obj$metadata[[input$group]])

      tagList(
        selectizeInput(ns('control_group'),
                       label = 'Control (baseline)',
                       choices = conds)
      )
    }) %>%
      bindEvent(input$group)

    output$opt_trt <- renderUI({

      conds <- unique(MTandem_obj$metadata[[input$group]])
      conds <- conds[conds != input$control_group]

      tagList(
        selectizeInput(ns('treatment_group'),
                       label = 'Treatment',
                       choices = conds)
      )
    }) %>%
      bindEvent(input$group,
                input$control_group)

    ## Calculating DA ----
    has_diff <- reactive({
      waiter_su$show()
      notid <- showNotification('Calculating differential analysis...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      MTandem_obj$differential_analysis(group = input$group,
                                        control_condition = input$control_group,
                                        treatment_condition = input$treatment_group)

      waiter_su$hide()

      !is.null(MTandem_obj$diff_table)
    }) %>%
      bindEvent(input$calculate)

    ## Box DA results ----
    observe({
      req(has_diff())
      output$diff_results <- renderUI({
        group_var <- MTandem_obj$get_groups()
        headerbox_factory('Differential analysis results',
                          status = 'success',
                          width = 12,
                          content = tagList(
                            fluidRow(
                              col_6(numericInput(ns('l2fc_thres'), 'Log2 Fold-Change Threshold',
                                                 value = 2, step = .5, min = 0)),
                              col_6(numericInput(ns('pval_thres'), 'P_value Threshold',
                                                 value = 0.05, step = .01,
                                                 min = 0, max = 1),
                                    checkboxInput(ns('use_pval_adj'),
                                                  'Use adjusted p-value',
                                                  value = FALSE))
                            ),
                            hr(),
                            fluidRow(
                              col_6(DT::dataTableOutput(ns('diff_table_filt')),
                                    hr(),
                                    shinyWidgets::downloadBttn(ns('down_diff'),
                                                               'Differential analysis results',
                                                               style = 'jelly',
                                                               color = 'warning',
                                                               block = TRUE,
                                                               size = 'xs',
                                                               icon = icon('file-export'))
                              ),
                              col_6(plotOutput(ns('volcano_plot')))
                            ),
                            hr(),
                            fluidRow(
                              col_3(h5(tags$strong('Figure options')),
                                    checkboxInput(ns('clust_feat_diff'),
                                                  'Cluster features',
                                                  value = FALSE),
                                    checkboxInput(ns('clust_samp_diff'),
                                                  'Cluster samples',
                                                  value = FALSE),
                                    selectizeInput(ns('color_by_diff'),
                                                   'Color by',
                                                   choices = group_var),
                                    uiOutput(ns('color_picker_diff')),
                                    shinyWidgets::actionBttn(ns('update_diff_fig'),
                                                             label = 'Update plot',
                                                             style = 'jelly',
                                                             color = 'primary',
                                                             size = 'sm',
                                                             block = TRUE)),
                              col_9(plotOutput(ns('diff_sig')),
                                    uiOutput(ns('num_sig_ft_diff')))
                            )

                          ))
      })
    }) %>%
      bindEvent(input$calculate)


    ## Table Significant DA metabolites ----
    observe({
      req(has_diff())
      output$diff_table_filt <- DT::renderDataTable({
        if(has_diff()){

          if(input$use_pval_adj){
            MTandem_obj$diff_table$pval_sel <- MTandem_obj$diff_table$pval_adj
          } else {
            MTandem_obj$diff_table$pval_sel <- MTandem_obj$diff_table$pval
          }


          MTandem_obj$diff_table %>%
            dplyr::filter(abs(log2FC) >= input$l2fc_thres,
                          pval_sel < input$pval_thres) %>%
            dplyr::select(-pval_sel)
        }
      },
      options = list(scrollY = TRUE,
                     dom = 'ti',
                     lineHeight = "50%"))

      output$volcano_plot <- renderPlot({
        MTandem_obj$plot_volcano(pval_thres = input$pval_thres,
                                 log2fc_thres = input$l2fc_thres,
                                 use_adjusted_pval = input$use_pval_adj)

      })
    }) %>%
      bindEvent(input$calculate,
                input$use_pval_adj)

    ## Get colors model plot ----
    pickers_diff <- reactive({
      val <- paste0('color_diff_', unique(MTandem_obj$metadata[[input$color_by_diff]]))
      purrr::map(val, function(x){
        label <- stringr::str_remove(x, 'color_diff_')
        colorpicker_factory(id = ns(x), label = label)
      })
    })

    output$color_picker_diff <- renderUI({
      pickers_diff()
    })

    group_colors_diff <- reactive({
      req(pickers_diff())
      from_picker <- unlist(as.character(pickers_diff()))
      init_colors <- stringr::str_extract(from_picker, '#.{6}')

      tr <- unique(MTandem_obj$metadata[[input$color_by_diff]])
      input_val <- paste0('color_mod_', unique(MTandem_obj$metadata[[input$color_by_diff]]))
      colors <- purrr::map2_chr(input_val, init_colors, ~purrr::`%||%`(input[[.x]], .y))
      names(colors) <- tr
      colors
    }) %>%
      bindEvent(input$calculate,
                input$update_diff_fig,
                input$color_by_diff)

    output$diff_sig <- renderPlot({
      if(has_diff()){

        MTandem_obj$plot_da_sig_features(pval_thres = input$pval_thres,
                                         log2fc_thres = input$l2fc_thres,
                                         use_adjusted_pval = input$use_pval_adj,
                                         color_by = input$color_by_diff,
                                         cluster_feat = input$clust_feat_diff,
                                         cluster_samp = input$clust_samp_diff,
                                         color_vector = group_colors_diff())

      }
    }) %>%
      bindEvent(input$calculate,
                input$update_diff_fig,
                input$use_pval_adj)

    output$num_sig_ft_diff <- renderUI({
      if(has_diff()){
        diff_table <- MTandem_obj$diff_table %>%
          dplyr::mutate(pval_sel = NA)

        if(input$use_pval_adj){
          diff_table$pval_sel <- diff_table$pval_adj
        } else {
          diff_table$pval_sel <- diff_table$pval
        }

        sig_fts <- diff_table %>%
          dplyr::filter(pval_sel < input$pval_thres,
                        abs(log2FC) >= input$l2fc_thres) %>%
          nrow()

        res <- paste0('A total of ', sig_fts,
                      ' features were differentially abundant')

        colored_text(res, 'green')
      }
    }) %>%
      bindEvent(input$calculate,
                input$use_pval_adj)

    ## Download DA table ----
    output$down_diff <- downloadHandler(
      filename = function(){
        paste('differential_analysis_results.tsv')
      },
      content = function(file){
        vroom::vroom_write(MTandem_obj$diff_table, file)
      }
    )


    # Model fitting server ----

    ## Select model variables ----
    output$model_params <- renderUI({
      group_var <- MTandem_obj$get_groups()
      if(input$model_type == 'lm'){
        tagList(
          col_6(shinyWidgets::virtualSelectInput(ns('lm_var'),
                                                 'Model variables',
                                                 choices = group_var,
                                                 multiple = TRUE,
                                                 showValueAsTags = TRUE))
        )
      } else if (input$model_type == 'lme'){
        tagList(
          col_6(shinyWidgets::virtualSelectInput(ns('fix_var'),
                                                 'Fixed effect variables',
                                                 choices = group_var,
                                                 multiple = TRUE,
                                                 showValueAsTags = TRUE)),
          col_6(shinyWidgets::virtualSelectInput(ns('random_var'),
                                                 'Random effect variables',
                                                 choices = group_var,
                                                 multiple = TRUE,
                                                 showValueAsTags = TRUE))
        )
      }
    })

    ## Contrast picker generator ----
    contrasts_pickers <- reactive({

      quick_df <- MTandem_obj$norm_abundance_table[1,] %>%
        tidyr::pivot_longer(all_of(MTandem_obj$metadata$SampleID),
                            names_to = 'SampleID',
                            values_to = 'value') %>%
        dplyr::left_join(MTandem_obj$metadata, by = 'SampleID')

      if(input$model_type == 'lm'){
        vars <- paste0(input$lm_var, collapse = '+')
        formula <- as.formula(paste0('value ~ ', vars))
        mod <- lm(formula, data = quick_df)

        coeff <- names(mod$coefficients) %>%
          stringr::str_remove_all('\\(|\\)')

        picker_list <- purrr::map(coeff, function(x){
          numpicker_factory(ns(paste0('contrast_', x, '_npc')),
                            label = x)
        })
      } else {

        fix_vars <- paste0(input$fix_var, collapse = '+')
        rand_vars <- paste0(paste0('(1|', input$random_var, ')'), collapse = '+')

        formula <- paste0('value ~ ', fix_vars, '+', rand_vars)

        mod <- lmerTest::lmer(formula, data = quick_df)

        coeff <- names(lme4::fixef(mod)) %>%
          stringr::str_remove_all('\\(|\\)')

        picker_list <- purrr::map(coeff, function(x){
          numpicker_factory(ns(paste0('contrast_', x, '_npc')),
                            label = x)
        })

      }

      picker_list

    }) %>%
      bindEvent(input$set_contrasts)

    output$bottom_params <- renderUI({
      tagList(
        fluidRow(
          purrr::map(contrasts_pickers(), col_2)
        ),
        fluidRow(
          col_6(),
          col_6(shinyWidgets::actionBttn(ns('fit'),
                                         label = 'Fit model',
                                         style = 'jelly',
                                         color = 'primary',
                                         size = 'sm',
                                         block = TRUE)))
      )
    }) %>%
      bindEvent(input$set_contrasts)

    ## Get colors model plot ----
    pickers_mod <- reactive({
      val <- paste0('color_mod_', unique(MTandem_obj$metadata[[input$color_by_mod]]))
      purrr::map(val, function(x){
        label <- stringr::str_remove(x, 'color_mod_')
        colorpicker_factory(id = ns(x), label = label)
      })
    })

    output$color_picker_mod <- renderUI({
      pickers_mod()
    })

    group_colors_mod <- reactive({
      req(pickers_mod())
      from_picker <- unlist(as.character(pickers_mod()))
      init_colors <- stringr::str_extract(from_picker, '#.{6}')

      tr <- unique(MTandem_obj$metadata[[input$color_by_mod]])
      input_val <- paste0('color_mod_', unique(MTandem_obj$metadata[[input$color_by_mod]]))
      colors <- purrr::map2_chr(input_val, init_colors, ~purrr::`%||%`(input[[.x]], .y))
      names(colors) <- tr
      colors
    }) %>%
      bindEvent(input$fit,
                input$update_mod_fig,
                input$color_by_mod)

    ## Model formula ----
    output$model_formula <- renderUI({

      if(input$model_type == 'lm'){
        vars <- paste0(input$lm_var, collapse = '+')
        formula <- paste0('Feature abundance ~ ', vars)
      } else if(input$model_type == 'lme'){
        fix_vars <- paste0(input$fix_var, collapse = '+')
        rand_vars <- paste0(paste0('(1|', input$random_var, ')'), collapse = '+')

        formula <- paste0('Feature abundance ~ ', fix_vars, '+', rand_vars)
      }

      colored_text(paste0('Fitted formula was: ', formula), 'green')
    }) %>%
      bindEvent(input$fit)

    contrast_vector <- reactive({
      all_inputs <- names(input)
      numpic_inputs <- all_inputs[which(stringr::str_detect(all_inputs, '_npc$'))]
      L <- purrr::map_int(numpic_inputs, function(x){
        l <- input[[x]]
        return(l)
      })
      L
    }) %>%
      bindEvent(input$fit)

    ## Fit model ----
    has_model <- reactive({
      req(contrast_vector())
      waiter_md$show()
      notid <- showNotification('Fitting models...',
                                duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(notid), add = TRUE)
      if(input$model_type == 'lm'){
        MTandem_obj$fit_models(model_type = input$model_type,
                               vars = input$lm_var)
      } else {
        MTandem_obj$fit_models(model_type = input$model_type,
                               fix_vars = input$fix_var,
                               rand_vars = input$random_var)
      }

      notid <- showNotification('Testing models...',
                                duration = NULL, closeButton = FALSE,
                                id = notid)
      MTandem_obj$check_model()

      notid <- showNotification('Calculating contrasts...',
                                duration = NULL, closeButton = FALSE,
                                id = notid)


      MTandem_obj$test_contrasts(L = contrast_vector())

      waiter_md$hide()

      !is.null(MTandem_obj$contrasts_results)
    }) %>%
      bindEvent(input$fit)

    ## Box model results ----
    observe({
      req(has_model())
      output$model_results <- renderUI({
        group_var <- MTandem_obj$get_groups()
        headerbox_factory('Model fitting results',
                          status = 'success',
                          width = 12,
                          content = tagList(
                            col_12(uiOutput(ns('model_formula')),
                                   hr(),
                                   plotOutput(ns('r2_plot'))),
                            fluidRow(
                              col_3(h5(tags$strong('Figure options')),
                                    checkboxInput(ns('clust_feat'),
                                                  'Cluster features',
                                                  value = FALSE),
                                    checkboxInput(ns('clust_samp'),
                                                  'Cluster samples',
                                                  value = FALSE),
                                    selectizeInput(ns('color_by_mod'),
                                                   'Color by',
                                                   choices = group_var),
                                    uiOutput(ns('color_picker_mod')),
                                    shinyWidgets::actionBttn(ns('update_mod_fig'),
                                                             label = 'Update plot',
                                                             style = 'jelly',
                                                             color = 'primary',
                                                             size = 'sm',
                                                             block = TRUE)),
                              col_9(plotOutput(ns('model_sig')),
                                    uiOutput(ns('num_sig_ft')))
                            ),
                            fluidRow(
                              col_6(shinyWidgets::downloadBttn(ns('down_checked_mod'),
                                                               'Model performance',
                                                               style = 'jelly',
                                                               color = 'warning',
                                                               block = TRUE,
                                                               size = 'xs',
                                                               icon = icon('file-export'))),
                              col_6(shinyWidgets::downloadBttn(ns('down_sig_mod'),
                                                               'Contrasts results',
                                                               style = 'jelly',
                                                               color = 'warning',
                                                               block = TRUE,
                                                               size = 'xs',
                                                               icon = icon('file-export')))
                            )
                          ))

      })
    }) %>%
      bindEvent(input$fit)

    # R2 distribution plot ----
    output$r2_plot <- renderPlot({
      if(has_model()){

        if(input$model_type == 'lm'){
          plot <- MTandem_obj$checked_models %>%
            dplyr::select(FeatureID, R2, R2_adjusted) %>%
            tidyr::pivot_longer(c(R2, R2_adjusted),
                                names_to = 'R2',
                                values_to = 'value') %>%
            ggplot2::ggplot() +
            ggplot2::geom_histogram(ggplot2::aes(x = value),
                                    fill = 'tan2',
                                    color = 'black') +
            ggplot2::facet_wrap(~R2, ncol = 2,
                                scales = 'free_x') +
            ggplot2::theme_bw()
        } else {
          plot <- MTandem_obj$checked_models %>%
            dplyr::select(FeatureID, R2_conditional, R2_marginal) %>%
            tidyr::pivot_longer(c(R2_conditional, R2_marginal),
                                names_to = 'R2',
                                values_to = 'value') %>%
            ggplot2::ggplot() +
            ggplot2::geom_histogram(ggplot2::aes(x = value),
                                    fill = 'tan2',
                                    color = 'black') +
            ggplot2::facet_wrap(~R2, ncol = 2,
                                scales = 'free_x') +
            ggplot2::theme_bw()
        }


        plot
      }
    }) %>%
      bindEvent(input$fit)

    # Download moder performance ----
    output$down_checked_mod <- downloadHandler(
      filename = function(){
        paste('models_performance.tsv')
      },
      content = function(file){
        vroom::vroom_write(MTandem_obj$checked_models, file)
      }
    )

    output$down_sig_mod <- downloadHandler(
      filename = function(){
        paste('contrasts_results.tsv')
      },
      content = function(file){
        vroom::vroom_write(MTandem_obj$contrasts_results, file)
      }
    )

    output$model_sig <- renderPlot({
      if(has_model()){

        MTandem_obj$plot_model_sig_features(color_by = input$color_by_mod,
                                            cluster_feat = input$clust_feat,
                                            cluster_samp = input$clust_samp,
                                            color_vector = group_colors_mod())

      }
    }) %>%
      bindEvent(input$fit,
                input$update_mod_fig)

    output$num_sig_ft <- renderUI({
      if(has_model()){
        sig_fts <- MTandem_obj$contrasts_results %>%
          dplyr::filter(adj.p.value < 0.05) %>%
          nrow()

        res <- paste0('A total of ', sig_fts,
                      ' features were significant with the specified contrast')

        colored_text(res, 'green')
      }
    }) %>%
      bindEvent(input$fit)

  })

}

## To be copied in the UI
# mod_stats_univ_ui("stats_univ_1")

## To be copied in the server
# mod_stats_univ_server("stats_univ_1")
