#' Differential analysis
#'
#' @description Function to perform differential abundance analysis among the specified groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
differential_analysis <- function(abun_table,
                                  metadata,
                                  norm_method,
                                  group,
                                  control_condition,
                                  treatment_condition) {

  control_samples <- metadata$SampleID[metadata[[group]] == control_condition]
  treatment_samples <- metadata$SampleID[metadata[[group]] == treatment_condition]


  norm_table <- apply_normalization(abun_table, norm_method = norm_method) %>%
    tibble::column_to_rownames(var = 'FeatureID')

  diff_table <- data.frame(FeatureID = rownames(norm_table)) %>%
    dplyr::mutate(control_means = NA,
                  treatment_means = NA,
                  log2FC = NA,
                  pval = NA)


  for(i in 1:nrow(norm_table)){
    ctr <- as.numeric(norm_table[i, control_samples])
    trt <- as.numeric(norm_table[i, treatment_samples])

    t_test <- t.test(ctr,
                     trt,
                     paired = FALSE)

    diff_table$control_means[i] <- mean(ctr)
    diff_table$treatment_means[i] <- mean(trt)
    diff_table$log2FC[i] <- log2(mean(trt)/mean(ctr))
    diff_table$pval[i] <- t_test$p.value

  }

  diff_table$pval_adj <- p.adjust(diff_table$pval, method = 'fdr')

  return(diff_table)
}


#' Volcano plot
#'
#' @description Function to perform differential abundance analysis among the specified groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
plot_volcano <- function(diff_table,
                         pval_thres,
                         log2fc_thres,
                         use_adjusted_pval = FALSE){

  diff_table <- diff_table %>%
    dplyr::mutate(pval_sel = NA)

  if(use_adjusted_pval){
    diff_table$pval_sel <- diff_table$pval_adj
  } else {
    diff_table$pval_sel <- diff_table$pval
  }

  plot <- diff_table %>%
    dplyr::mutate(reg = dplyr::case_when(
      pval_sel < pval_thres & log2FC >= log2fc_thres ~ 'Up',
      pval_sel < pval_thres & log2FC <= -log2fc_thres ~ 'Down',
      TRUE ~ 'None'
    )) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = log2FC,
                                     y = -log10(pval_sel),
                                     fill = reg),
                        shape = 21,
                        show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = -log10(pval_thres),
                        linetype = 2) +
    ggplot2::geom_vline(xintercept = c(log2fc_thres, - log2fc_thres),
                        linetype = 2) +
    ggplot2::scale_fill_manual(values = c('Up' = 'indianred2',
                                          'Down' = 'steelblue',
                                          'None' = 'gray')) +
    ggplot2::labs(x = 'Log2 Fold-Change',
                  y = '-log10(p_value)') +
    ggplot2::theme_bw() +
    ggplot2::theme()

  return(plot)
}

#' Heatmap significant features
#'
#' @description Function to check the performance of the models that were fitted.
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
plot_da_sig_features <- function(abundance_table,
                                 metadata,
                                 diff_table,
                                 pval_thres,
                                 log2fc_thres,
                                 color_by,
                                 use_adjusted_pval = FALSE,
                                 cluster_feat = FALSE,
                                 cluster_samp = FALSE,
                                 color_vector = NULL){

  diff_table <- diff_table %>%
    dplyr::mutate(pval_sel = NA)

  if(use_adjusted_pval){
    diff_table$pval_sel <- diff_table$pval_adj
  } else {
    diff_table$pval_sel <- diff_table$pval
  }

  diff_table_filt <- diff_table %>%
    dplyr::filter(pval_sel < pval_thres,
                  abs(log2FC) >= log2fc_thres)

  metadata_tile <- metadata %>%
    ggplot2::ggplot(ggplot2::aes(y = 1,
                                 x = SampleID,
                                 fill = .data[[color_by]])) +
    ggplot2::geom_tile(color = 'white') +
    ggplot2::theme_void()

  if(!is.null(color_vector)){
    metadata_tile <- metadata_tile +
      ggplot2::scale_fill_manual(values = color_vector)
  }

  htmp_plot <- abundance_table %>%
    tidyr::pivot_longer(!FeatureID,
                        names_to = 'SampleID',
                        values_to = 'value') %>%
    dplyr::filter(FeatureID %in% diff_table_filt$FeatureID) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = SampleID,
                                    y = FeatureID,
                                    fill = value),
                       color = 'white') +
    ggplot2::labs(fill = 'Norm. Intensity') +
    ggplot2::scale_fill_viridis_c(option = 'A', direction = -1) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90),
                   panel.grid = ggplot2::element_blank())

  if(cluster_feat){
    dendro_axis_ft <- abundance_table %>%
      dplyr::filter(FeatureID %in% diff_table_filt$FeatureID) %>%
      tibble::column_to_rownames(var = 'FeatureID') %>%
      dist(.) %>%
      hclust(.)

    htmp_plot <- htmp_plot +
      legendry::scale_y_dendro(clust = dendro_axis_ft,
                               guide = legendry::guide_axis_dendro())
  }

  if(cluster_samp){
    dendro_axis_samp <- abundance_table %>%
      dplyr::filter(FeatureID %in% diff_table_filt$FeatureID) %>%
      tibble::column_to_rownames(var = 'FeatureID') %>%
      t(.) %>%
      dist(.) %>%
      hclust(.)

    htmp_plot <- htmp_plot +
      ggplot2::guides(x = 'axis',
                      x.sec = legendry::guide_axis_dendro(dendro_axis_samp,
                                                          labels = FALSE)) +
      ggplot2::theme(axis.ticks.x.top = ggplot2::element_blank(),
                     axis.ticks.length.x.top = ggplot2::unit(0, 'lines'))


    metadata_tile <- metadata_tile  +
      ggplot2::guides(x = 'axis',
                      x.sec = legendry::guide_axis_dendro(dendro_axis_samp,
                                                          labels = FALSE)) +
      ggplot2::theme(axis.ticks.x.top = ggplot2::element_line(),
                     axis.ticks.length.x.top = ggplot2::unit(.2, 'lines'))
  }

  final_plot <- patchwork::wrap_plots(list(metadata_tile,
                                           htmp_plot),
                                      ncol = 1,
                                      heights = c(.1, 1),
                                      guides = 'collect')

  return(final_plot)

}

#' Fit linear models
#'
#' @description Function to perform differential abundance analysis among the specified groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @rdname fit_models
#' @export
fit_model <- function(abun_table,
                      metadata,
                      model_type,
                      vars = NULL,
                      fix_vars = NULL,
                      rand_vars = NULL){

  feature_table <- abun_table %>%
    tidyr::pivot_longer(dplyr::all_of(metadata$SampleID),
                        names_to = 'SampleID',
                        values_to = 'int') %>%
    dplyr::left_join(metadata, by = 'SampleID')

  if(model_type == 'lm'){
    model_df <- fit_model.lm(feature_table = feature_table,
                             vars = vars)
  } else if(model_type == 'lme'){
    model_df <- fit_model.lme(feature_table,
                              fix_vars = fix_vars,
                              rand_vars = rand_vars)
  }

  return(model_df)

}

#' @rdname fit_models
#' @export
fit_model.lm <- function(feature_table,
                         vars){

  vars <- paste0(vars, collapse = '+')
  formula <- as.formula(paste0('int ~ ', vars))

  model_fit <- feature_table %>%
    dplyr::group_by(FeatureID) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model = purrr::map(data, function(df){
        mod <- lm(formula = formula, data = df)
      })
    )

  return(model_fit)

}

#' @rdname fit_models
#' @export
fit_model.lme <- function(feature_table,
                          fix_vars,
                          rand_vars){

  fix_vars <- paste0(fix_vars, collapse = '+')
  rand_vars <- paste0(paste0("(1|", rand_vars, ")"), collapse = '+')
  formula <- as.formula(paste0('int ~ ', fix_vars, '+', rand_vars))

  safe_lmer <- purrr::safely(lmerTest::lmer, otherwise = 'Model not worked')

  model_fit <- feature_table %>%
    dplyr::group_by(FeatureID) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model = purrr::map(data, function(df){
        res <- safe_lmer(formula = formula, data = df)

        if(is.null(res$error)){
          return(res$result)
        } else {
          return(res$error)
        }
      })
    )

  return(model_fit)
}


#' Check model performance
#'
#' @description Function to check the performance of the models that were fitted.
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
check_model <- function(model_df){
  model_df <- model_df %>%
    dplyr::mutate(
      perf_model = purrr::map(model, function(mod){
        perf = performance::model_performance(mod)
      })
    ) %>%
    tidyr::unnest(perf_model) %>%
    dplyr::select(-data, -model)

  return(model_df)
}


#' Test contrasts
#'
#' @description Function to check the performance of the models that were fitted.
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
test_contrasts <- function(model_df,
                           L){

  if(class(model_df$model[[1]]) == 'lm'){
    model_df <- model_df %>%
      dplyr::mutate(
        contrast = purrr::map(model, function(mod){
          test <- multcomp::glht(mod, linfct = matrix(L, 1))
          res <- summary(test) %>%
            broom::tidy()
        })
      ) %>%
      tidyr::unnest(contrast) %>%
      dplyr::select(-data, -model)
  } else {

    model_df <- model_df %>%
      dplyr::mutate(
        contrast = purrr::map(model, function(mod){
          lmerTest::contest(mod, L = L)
        })
      ) %>%
      tidyr::unnest(contrast) %>%
      dplyr::select(-data, -model) %>%
      dplyr::mutate(adj.p.value = p.adjust(`Pr(>F)`, method = 'fdr'))
  }



  return(model_df)
}


#' Heatmap significant features
#'
#' @description Function to check the performance of the models that were fitted.
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
plot_model_sig_features <- function(abundance_table,
                                    metadata,
                                    contrasts_results,
                                    color_by,
                                    cluster_feat = FALSE,
                                    cluster_samp = FALSE,
                                    color_vector = NULL){

  sig_fts <- contrasts_results %>%
    dplyr::filter(adj.p.value < 0.05) %>%
    dplyr::pull(FeatureID)

  metadata_tile <- metadata %>%
    ggplot2::ggplot(ggplot2::aes(y = 1,
                                 x = SampleID,
                                 fill = .data[[color_by]])) +
    ggplot2::geom_tile(color = 'white') +
    ggplot2::theme_void()

  if(!is.null(color_vector)){
    metadata_tile <- metadata_tile +
      ggplot2::scale_fill_manual(values = color_vector)
  }

  htmp_plot <- abundance_table %>%
    tidyr::pivot_longer(!FeatureID,
                        names_to = 'SampleID',
                        values_to = 'value') %>%
    dplyr::filter(FeatureID %in% sig_fts) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = SampleID,
                                    y = FeatureID,
                                    fill = value),
                       color = 'white') +
    ggplot2::labs(fill = 'Norm. Intensity') +
    ggplot2::scale_fill_viridis_c(option = 'A', direction = -1) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90),
                   panel.grid = ggplot2::element_blank())

  if(cluster_feat){
    dendro_axis_ft <- abundance_table %>%
      dplyr::filter(FeatureID %in% sig_fts) %>%
      tibble::column_to_rownames(var = 'FeatureID') %>%
      dist(.) %>%
      hclust(.)

    htmp_plot <- htmp_plot +
      legendry::scale_y_dendro(clust = dendro_axis_ft,
                               guide = legendry::guide_axis_dendro())
  }

  if(cluster_samp){
    dendro_axis_samp <- abundance_table %>%
      dplyr::filter(FeatureID %in% sig_fts) %>%
      tibble::column_to_rownames(var = 'FeatureID') %>%
      t(.) %>%
      dist(.) %>%
      hclust(.)

    htmp_plot <- htmp_plot +
      ggplot2::guides(x = 'axis',
                      x.sec = legendry::guide_axis_dendro(dendro_axis_samp,
                                                          labels = FALSE)) +
      ggplot2::theme(axis.ticks.x.top = ggplot2::element_blank(),
                     axis.ticks.length.x.top = ggplot2::unit(0, 'lines'))


    metadata_tile <- metadata_tile  +
      ggplot2::guides(x = 'axis',
                      x.sec = legendry::guide_axis_dendro(dendro_axis_samp,
                                                          labels = FALSE)) +
      ggplot2::theme(axis.ticks.x.top = ggplot2::element_line(),
                     axis.ticks.length.x.top = ggplot2::unit(.2, 'lines'))
  }

  final_plot <- patchwork::wrap_plots(list(metadata_tile,
                                           htmp_plot),
                                      ncol = 1,
                                      heights = c(.1, 1),
                                      guides = 'collect')

  return(final_plot)

}


