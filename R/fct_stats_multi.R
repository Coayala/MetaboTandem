#' @title Multivariate analysis - Ordination
#' @description Function to calculate ordination with different methods
#' @param abun_table Feature abundance table
#' @param method Ordination method
#' @param distance Dissimilarity distance to calculate. Available options are
#'  those from `vegan::vegdist()`
#' @return Object returned depend on the selected method
#' @rdname ordination_methods
#' @export
calculate_ordination <- function(abun_table,
                                 method,
                                 distance = 'bray',
                                 group = NULL){

  abun_table <- abun_table %>%
    tibble::column_to_rownames(var = 'FeatureID')

  ord <- switch(method,
                nmds = nmds_ordination(abun_table, distance),
                pca = pca_ordination(abun_table),
                pcoa = pcoa_ordination(abun_table, distance))

}

#' @rdname ordination_methods
#' @export
nmds_ordination <- function(abun_table, distance){

  nmds <- vegan::metaMDS(t(abun_table),
                         k = 2,
                         distance = distance,
                         autotransform = FALSE,
                         maxit = 999,
                         trymax = 500,
                         wascores = TRUE)

  return(nmds)

}


#' @rdname ordination_methods
#' @export
pca_ordination <- function(abun_table){

  pca <- prcomp(t(abun_table), center = TRUE)

  return(pca)
}

#' @rdname ordination_methods
#' @export
pcoa_ordination <- function(abun_table, distance){

  dist_mat <- vegan::vegdist(t(abun_table), distance)
  pcoa <- vegan::wcmdscale(dist_mat, eig = TRUE)

  return(pcoa)
}


#' @title Multivariate analysis - Plot Ordination
#' @description Function to plot ordination results
#' @param ord_onj Feature abundance table
#' @param method Ordination method
#' @param distance Dissimilarity distance to calculate. Available options are
#'  those from `vegan::vegdist()`
#' @return Object returned depend on the selected method
#' @rdname ordination_plot
#' @export
plot_ordination <- function(ord_object,
                            metadata,
                            group_by,
                            add_sample_names = FALSE,
                            add_variables = FALSE,
                            add_ellipse = FALSE,
                            color_vector = NULL,
                            abundances = NULL,
                            plot = TRUE){
  if(class(ord_object)[1] == 'metaMDS'){
    res <- plot_ordination.nmds(ord_object,
                                metadata,
                                group_by,
                                add_sample_names,
                                add_variables,
                                add_ellipse,
                                color_vector,
                                abundances,
                                plot)
  } else if(class(ord_object)[1] == 'prcomp'){
    res <- plot_ordination.pca(ord_object,
                               metadata,
                               group_by,
                               add_sample_names,
                               add_variables,
                               add_ellipse,
                               color_vector,
                               plot)
  } else if(class(ord_object)[1] == 'wcmdscale'){
    res <- plot_ordination.pcoa(ord_object,
                                metadata,
                                group_by,
                                add_sample_names,
                                add_variables,
                                add_ellipse,
                                color_vector,
                                abundances,
                                plot)
  }

  return(res)
}

#' @rdname ordination_plot
#' @export
plot_ordination.nmds <- function(ord_object,
                                 metadata,
                                 group_by,
                                 add_sample_names,
                                 add_variables,
                                 add_ellipse,
                                 color_vector,
                                 abundances,
                                 plot){

  # Extracting scores
  nmds_scores <- vegan::scores(ord_object, display = c('sites'), tidy = TRUE) %>%
    dplyr::select(SampleID = label, NMDS1, NMDS2) %>%
    dplyr::left_join(metadata, by = 'SampleID')

  if(add_variables){
    abundances <- abundances %>%
      tibble::column_to_rownames(var = 'FeatureID') %>%
      t()
    fit_obj <- vegan::envfit(ord_object, abundances)$vectors
    nmds_fit_all <- data.frame(fit_obj$arrows) %>%
      tibble::rownames_to_column(var = 'FeatureID') %>%
      dplyr::mutate(r2 = fit_obj$r,
                    pval = fit_obj$pvals)

    nmds_fit <- nmds_fit_all %>%
      dplyr::filter(pval < 0.05)
  }

  if(plot){
    ord_plot <- nmds_scores %>%
      ggplot2::ggplot(ggplot2::aes(x = NMDS1,
                                   y = NMDS2,
                                   color = .data[[group_by]])) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = 'bottom')

    if(add_sample_names){
      ord_plot <- ord_plot +
        ggplot2::geom_text(ggplot2::aes(label = SampleID),
                           position = ggplot2::position_jitter(seed = 123))
    }

    if(add_ellipse){
      ord_plot <- ord_plot +
        ggforce::geom_mark_ellipse(ggplot2::aes(group = .data[[group_by]],
                                                color = .data[[group_by]]),
                                   fill = 'transparent',
                                   show.legend = FALSE,
                                   alpha = 0.3)
    }

    if(add_variables){

      scale <- max(abs(c(nmds_fit$NMDS1, nmds_fit$NMDS2))) /
        max(abs(c(nmds_scores$NMDS1, nmds_scores$NMDS2)))

      jitter <- (max(c(nmds_scores$NMDS1, nmds_scores$NMDS2)) -
                   min(c(nmds_scores$NMDS1, nmds_scores$NMDS2))) / 50

      ord_plot <- ord_plot +
        ggplot2::geom_segment(data = nmds_fit,
                              ggplot2::aes(x = 0,
                                           y = 0,
                                           xend = NMDS1 / scale,
                                           yend = NMDS2 / scale),
                              color = 'gray70',
                              arrow = ggplot2::arrow(type = 'closed',
                                                     length = ggplot2::unit(.2, "lines")),
                              linewidth = .1,
                              inherit.aes = FALSE) +
        ggplot2::geom_text(data = nmds_fit,
                           ggplot2::aes(x = NMDS1 / scale,
                                        y = NMDS2 / scale,
                                        label = FeatureID),
                           size = 2,
                           color = 'black',
                           position = ggplot2::position_jitter(seed = 123,
                                                               height = jitter,
                                                               width = jitter),
                           inherit.aes = FALSE)
    }

    if(!is.null(color_vector)){
      ord_plot <- ord_plot +
        ggplot2::scale_color_manual(values = color_vector)
    }

    return(ord_plot)

  } else {
    if(add_variables){
      res <- list(sample_scores = nmds_scores,
                  feature_scores = nmds_fit)
    } else {
      res <- list(sample_scores = nmds_scores)
    }

    return(res)
  }

}

#' @rdname ordination_plot
#' @export
plot_ordination.pca <- function(ord_object,
                                metadata,
                                group_by,
                                add_sample_names,
                                add_variables,
                                add_ellipse,
                                color_vector,
                                plot){

  pca_scores <- as.data.frame(ord_object$x) %>%
    tibble::rownames_to_column(var = 'SampleID') %>%
    dplyr::left_join(metadata, by = 'SampleID')

  if(add_variables){
    pca_loadings <- factoextra::facto_summarize(ord_object,
                                                element = "var",
                                                result=c("coord","contrib","cos2"),
                                                axes=c(1,2),
                                                select = list(contrib = 20)) %>%
      dplyr::rename(FeatureID = name)
  }

  if(plot){

    eigen <- factoextra::get_eig(ord_object)

    PC1 <- paste0('PC1 [', round(eigen$variance.percent[1], 2),
                  '%]')
    PC2 <- paste0('PC2 [', round(eigen$variance.percent[2], 2),
                  '%]')

    ord_plot <- pca_scores %>%
      ggplot2::ggplot(ggplot2::aes(x = PC1,
                                   y = PC2,
                                   color = .data[[group_by]])) +
      ggplot2::geom_point(size = 3) +
      ggplot2::labs(x = PC1,
                    y = PC2) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = 'bottom')

    if(add_sample_names){
      ord_plot <- ord_plot +
        ggplot2::geom_text(ggplot2::aes(label = SampleID),
                           position = ggplot2::position_jitter(seed = 123))
    }

    if(add_ellipse){
      ord_plot <- ord_plot +
        ggforce::geom_mark_ellipse(ggplot2::aes(group = .data[[group_by]],
                                                color = .data[[group_by]]),
                                   fill = 'transparent',
                                   show.legend = FALSE,
                                   alpha = 0.3)
    }

    if(add_variables){

      scale <- max(abs(c(pca_loadings$Dim.1, pca_loadings$Dim.2))) /
        max(abs(c(pca_scores$PC1, pca_scores$PC2)))

      jitter <- (max(c(pca_scores$PC1, pca_scores$PC2)) -
                   min(c(pca_scores$PC1, pca_scores$PC2))) / 50

      ord_plot <- ord_plot +
        ggplot2::geom_segment(data = pca_loadings,
                              ggplot2::aes(x = 0,
                                           y = 0,
                                           xend = Dim.1 / scale,
                                           yend = Dim.2 / scale),
                              color = 'gray70',
                              arrow = ggplot2::arrow(type = 'closed',
                                                     length = ggplot2::unit(.2, "lines")),
                              linewidth = .1,
                              inherit.aes = FALSE) +
        ggplot2::geom_text(data = pca_loadings,
                           ggplot2::aes(x = Dim.1 / scale,
                                        y = Dim.2 / scale,
                                        label = FeatureID),
                           size = 2,
                           color = 'black',
                           position = ggplot2::position_jitter(seed = 123,
                                                               height = jitter,
                                                               width = jitter),
                           inherit.aes = FALSE)
    }

    if(!is.null(color_vector)){
      ord_plot <- ord_plot +
        ggplot2::scale_color_manual(values = color_vector)
    }

    return(ord_plot)

  } else {
    if(add_variables){
      res <- list(sample_scores = pca_scores,
                  feature_scores = pca_loadings)
    } else {
      res <- list(sample_scores = pca_scores)
    }

    return(res)
  }

}

#' @rdname ordination_plot
#' @export
plot_ordination.pcoa <- function(ord_object,
                                 metadata,
                                 group_by,
                                 add_sample_names,
                                 add_variables,
                                 add_ellipse,
                                 color_vector,
                                 abundances,
                                 plot){

  pcoa_scores <- as.data.frame(ord_object$points) %>%
    tibble::rownames_to_column(var = 'SampleID') %>%
    dplyr::left_join(metadata, by = 'SampleID')

  if(add_variables){
    abundances <- abundances %>%
      as.data.frame() %>%
      tibble::column_to_rownames(var = 'FeatureID') %>%
      t()

    fit_obj <- vegan::envfit(ord_object, abundances)$vectors
    pcoa_fit_all <- data.frame(fit_obj$arrows) %>%
      tibble::rownames_to_column(var = 'FeatureID') %>%
      dplyr::mutate(r2 = fit_obj$r,
                    pval = fit_obj$pvals)

    pcoa_fit <- pcoa_fit_all %>%
      dplyr::filter(pval < 0.05)
  }

  if(plot){

    eigen <- ord_object$eig / sum(ord_object$eig) * 100

    PCoA1 <- paste0('PCoA1 [', round(eigen[1], 2), '%]')
    PCoA2 <- paste0('PCoA2 [', round(eigen[2], 2), '%]')


    ord_plot <- pcoa_scores %>%
      ggplot2::ggplot(ggplot2::aes(x = Dim1,
                                   y = Dim2,
                                   color = .data[[group_by]])) +
      ggplot2::geom_point(size = 3) +
      ggplot2::labs(x = PCoA1,
                    y = PCoA2) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = 'bottom')

    if(add_sample_names){
      ord_plot <- ord_plot +
        ggplot2::geom_text(ggplot2::aes(label = SampleID),
                           position = ggplot2::position_jitter(seed = 123))
    }

    if(add_ellipse){
      ord_plot <- ord_plot +
        ggforce::geom_mark_ellipse(ggplot2::aes(group = .data[[group_by]],
                                                color = .data[[group_by]]),
                                   fill = 'transparent',
                                   show.legend = FALSE,
                                   alpha = 0.3)
    }

    if(add_variables){

      scale <- max(abs(c(pcoa_fit$Dim1, pcoa_fit$Dim2))) /
        max(abs(c(pcoa_scores$Dim1, pcoa_scores$Dim2)))

      jitter <- (max(c(pcoa_scores$Dim1, pcoa_scores$Dim2)) -
                   min(c(pcoa_scores$Dim1, pcoa_scores$Dim2))) / 50

      ord_plot <- ord_plot +
        ggplot2::geom_segment(data = pcoa_fit,
                              ggplot2::aes(x = 0,
                                           y = 0,
                                           xend = Dim1 / scale,
                                           yend = Dim2 / scale),
                              color = 'gray70',
                              arrow = ggplot2::arrow(type = 'closed',
                                                     length = ggplot2::unit(.2, "lines")),
                              linewidth = .1,
                              inherit.aes = FALSE) +
        ggplot2::geom_text(data = pcoa_fit,
                           ggplot2::aes(x = Dim1 / scale,
                                        y = Dim2 / scale,
                                        label = FeatureID),
                           size = 2,
                           color = 'black',
                           position = ggplot2::position_jitter(seed = 123,
                                                               height = jitter,
                                                               width = jitter),
                           inherit.aes = FALSE)
    }

    if(!is.null(color_vector)){
      ord_plot <- ord_plot +
        ggplot2::scale_color_manual(values = color_vector)
    }

    return(ord_plot)

  } else {
    if(add_variables){
      res <- list(sample_scores = pcoa_scores,
                  feature_scores = pcoa_fit)
    } else {
      res <- list(sample_scores = pcoa_scores)
    }

    return(res)
  }

}


#' @title Multivariate analysis - PERMANOVA
#' @description Function to perform a permutational analysis of variance
#' @param abun_table Feature abundance table with features as rows and samples as columns
#' @param vars Independent variables
#' @param distance Dissimilarity distance to calculate. Available options are
#' those from `vegan::vegdist()`
#' @param use_interaction Calculate interaction of selected variables
#' @param strata Variable to define sample blocks
#'
#' @return Object returned depend on the selected method
#' @export
calculate_permanova<- function(abun_table,
                               metadata,
                               vars,
                               distance = 'bray',
                               assess = 'model',
                               use_interaction = FALSE,
                               strata = NULL){

  abun_table <- abun_table %>%
    tibble::column_to_rownames(var = 'FeatureID') %>%
    t()

  metadata <- metadata %>%
    tibble::column_to_rownames(var = 'SampleID')

  vars <- paste0(vars, collapse = '+')

  if(use_interaction){

    vars <- paste0('(', vars, ')^2')
  }

  formula <- as.formula(paste0('abun_table ~ ', vars))

  if(assess == 'model'){
    res <- vegan::adonis2(formula,
                          data = metadata,
                          method = distance,
                          permutations = 999,
                          strata = strata)
  } else {
    res <- vegan::adonis2(formula,
                          data = metadata,
                          method = distance,
                          permutations = 999,
                          strata = strata,
                          by = assess)
  }

  return(res)
}

#' @title Multivariate analysis - Clustering
#' @description Function to calculate ordination with different methods
#' @param abun_table Feature abundance table
#' @param method Clustering method
#' @param k Selecte K (for kmeans method)
#' @export
calculate_clustering <- function(abun_table,
                                 metadata,
                                 color_by,
                                 distance = 'euclidean',
                                 cluster_algorithm = 'ward.D2',
                                 add_kmeans = FALSE,
                                 k = 3,
                                 plot = TRUE,
                                 color_vector = NULL){
  abun_table <- abun_table %>%
    tibble::column_to_rownames(var = 'FeatureID')

  dist_mat <- dist(t(abun_table), method = distance)

  hclust_obj <- hclust(dist_mat, method = cluster_algorithm)

  if(add_kmeans){

    kmeans_obj <- kmeans(t(abun_table), centers = k)
  }

  if(plot){

    annot_df <- ggtree::ggtree(hclust_obj, branch.length = 'none')$data %>%
      dplyr::left_join(metadata, by = c('label' = 'SampleID')) %>%
      tidyr::drop_na(label)

    metadata_tile <- annot_df %>%
      ggplot2::ggplot(ggplot2::aes(y = 1,
                                   x = y,
                                   fill = .data[[color_by]])) +
      ggplot2::geom_tile(color = 'white') +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = 'bottom')

    if(!is.null(color_vector)){
      metadata_tile <- metadata_tile +
        ggplot2::scale_fill_manual(values = color_vector)
    }

    dend_plot <- ggtree::ggtree(hclust_obj) +
      ggtree::layout_dendrogram() +
      ggtree::geom_tiplab(size = 2,
                          ggplot2::aes(x = x + .1))

    final_plot <- patchwork::wrap_plots(list(dend_plot, metadata_tile),
                                        ncol = 1,
                                        heights = c(1.1, .1),
                                        guides = 'collect') &
      ggplot2::theme(legend.position = 'bottom')

    if(add_kmeans){

      kmeans_df <- data.frame(label = names(kmeans_obj$cluster),
                              cluster = as.character(kmeans_obj$cluster))

      kmeans_annot <- ggtree::ggtree(hclust_obj, branch.length = 'none')$data %>%
        dplyr::left_join(kmeans_df, by = 'label') %>%
        tidyr::drop_na(label)

      kmeans_tile <- kmeans_annot %>%
        ggplot2::ggplot(ggplot2::aes(y = 1,
                                     x = y,
                                     fill = cluster)) +
        ggplot2::geom_tile(color = 'white') +
        ggplot2::scale_x_discrete(expand = c(0, 0)) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = 'bottom') +
        ggplot2::scale_fill_manual(values = ggpubr::get_palette('Dark2', k))

      final_plot <- patchwork::wrap_plots(list(dend_plot,
                                               metadata_tile,
                                               kmeans_tile),
                                          ncol = 1,
                                          heights = c(1, .1, .1),
                                          guides = 'collect') &
        ggplot2::theme(legend.position = 'bottom')
    }

    return(final_plot)
  } else {
    if(add_kmeans){
      return(list(hclust = hclust_obj,
                  kmeans = kmeans_obj))
    } else {
      return(list(hclust = hclust_obj))
    }
  }
}
