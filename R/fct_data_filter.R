#' Filter by prevalence
#'
#' Function to filter features based on prevalence
#'
#'
#' @param abun_table Matrix or data frame with the unnormalized peak intensities
#'               with peaks as rows and samples as columns
#' @param min_perc_samples Minimum percentage of samples a feature need to be
#'        present
#'
#' @rdname data_filtering
#' @export
apply_prevalence_filtering <- function(abun_table,
                                       min_perc_samples){

  abun_table <- abun_table %>%
    tibble::column_to_rownames(var = 'FeatureID')

  num_samples <- round(ncol(abun_table) * min_perc_samples/100)

  filt_abun_table <- abun_table[rowSums(abun_table > 0) >= num_samples,]

  filt_abun_table <- filt_abun_table %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'FeatureID')

  return(filt_abun_table)

}

#' Filter low variance data
#'
#' Function to filter features with low variances
#'
#'
#' @param abun_table abun_table or data frame with the non-normalized peak intensities
#'               with peaks as rows and samples as columns
#' @param filter_method Filter method to apply
#' @param perc_remove Percentage of features to remove from the analysis
#'
#' @rdname data_filtering
#' @export
apply_variance_filtering <- function(abun_table,
                                     filter_method,
                                     perc_remove){
  abun_table <- abun_table %>%
    tibble::column_to_rownames(var = 'FeatureID')

  filt_abun_table <- switch(filter_method,
                            iqr = iqr_filtering(abun_table, perc_remove),
                            sd = sd_filtering(abun_table, perc_remove),
                            mad = mad_filtering(abun_table, perc_remove),
                            rsd = rsd_filtering(abun_table, perc_remove),
                            none = abun_table)

  filt_abun_table <- filt_abun_table %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'FeatureID')

  return(filt_abun_table)

}


#' @rdname data_filtering
#' @export
iqr_filtering <- function(abun_table, perc_remove){

  iqrs <- matrixStats::rowIQRs(as.matrix(abun_table))
  feature_rank <- rank(-iqrs, ties.method = 'random')
  num_remaining <- round(nrow(abun_table)*(1-perc_remove/100))

  filt_abun_table <- abun_table[feature_rank <= num_remaining,]

  return(filt_abun_table)

}


#' @rdname data_filtering
#' @export
sd_filtering <- function(abun_table, perc_remove){

  sds <- matrixStats::rowSds(as.matrix(abun_table))
  feature_rank <- rank(-sds, ties.method = 'random')
  num_remaining <- round(nrow(abun_table)*(1-perc_remove/100))

  filt_abun_table <- abun_table[feature_rank <= num_remaining,]

  return(filt_abun_table)

}

#' @rdname data_filtering
#' @export
mad_filtering <- function(abun_table, perc_remove){

  mads <- matrixStats::rowMads(as.matrix(abun_table))
  feature_rank <- rank(-mads, ties.method = 'random')
  num_remaining <- round(nrow(abun_table)*(1-perc_remove/100))

  filt_abun_table <- abun_table[feature_rank <= num_remaining,]

  return(filt_abun_table)

}

#' @rdname data_filtering
#' @export
rsd_filtering <- function(abun_table, perc_remove){

  sds <- matrixStats::rowSds(as.matrix(abun_table))
  means <- colMeans(as.matrix(abun_table))
  rsds <- sds/means
  feature_rank <- rank(-rsds, ties.method = 'random')
  num_remaining <- round(nrow(abun_table)*(1-perc_remove/100))

  filt_abun_table <- abun_table[feature_rank <= num_remaining,]

  return(filt_abun_table)

}
