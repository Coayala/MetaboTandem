#' metaclean
#'
#' @description A fct function
#'
#' @importFrom magrittr `%<>%`
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
apply_cleaning <- function(data,
                           group_by,
                           model_file) {

  # Transforming into an xcms object
  xcms_set <- as(xcms::filterMsLevel(data, msLevel = 1), 'xcmsSet')
  xcms::sampclass(xcms_set) <- data@sampleData[[group_by]]

  # xcms gap filling
  xcms_fill <- xcms::fillPeaks(xcms_set)

  # Checking peaks present in xcmsSet and MsExperiment objects
  ft_def <- xcms::featureDefinitions(data)
  peak_group_names_available <- data.frame(
    FeatureID = rownames(ft_def),
    group_name = xcms::groupnames(xcms_set)
  ) %>%
    dplyr::filter(group_name %in% xcms::groupnames(xcms_fill))

  # Applying metaclean
  available_peaks <- stringr::str_remove(peak_group_names_available$FeatureID, 'FT') %>%
    as.numeric

  all_eic <- xcms::getEIC(xcms_set,
                          groupidx = available_peaks,
                          rt = 'corrected')

  all_eval <- MetaClean::getEvalObj(xs = all_eic,
                                    fill = xcms_fill)

  fill_group <- xcms::groupnames(xcms_fill)

  all_peak_quality <- MetaClean::getPeakQualityMetrics(eicEvalData = all_eval)

  fill_gnames <- fill_group[all_peak_quality$EICNo]

  all_peak_quality_filt <- all_peak_quality %>%
    dplyr::mutate(group_name = fill_gnames) %>%
    dplyr::left_join(peak_group_names_available) %>%
    tidyr::drop_na()

  metaclean_model <- readr::read_rds(model_file)

  all_predictions <- MetaClean::getPredicitons(model = metaclean_model,
                                               testData = all_peak_quality_filt,
                                               eicColumn = 'FeatureID')

  pass_metabolites <- all_predictions %>%
    dplyr::filter(Pred_Class == 'Pass') %>%
    dplyr::mutate(FeatureID = EIC)

  data_filt <- xcms::filterFeatureDefinitions(data,
                                              features = pass_metabolites$FeatureID)

  return(data_filt)
}


#' Prepare data for training a metaclean model
#'
#' @description Function to prepare dataset for training a metaclean model
#'
#' @importFrom magrittr `%<>%`
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
prepare_for_cleaning <- function(data, metadata, n_subset){

  feature_table <- extract_feature_definitions(data)

  ft_def <- xcms::featureDefinitions(data)

  xcms_set <- as(xcms::filterMsLevel(data, msLevel = 1), 'xcmsSet')
  xcms::sampclass(xcms_set) <- metadata[[2]]

  xcms_fill <- xcms::fillPeaks(xcms_set)

  peak_group_names_available <- data.frame(
    FeatureID = rownames(ft_def),
    group_name = xcms::groupnames(xcms_set)
  ) %>%
    dplyr::filter(group_name %in% xcms::groupnames(xcms_fill))
  # Select peaks and extract eics

  set.seed(123)
  sel_peaks <- sample(peak_group_names_available$FeatureID, n_subset) %>%
    stringr::str_remove('FT') %>%
    as.numeric

  chroms <- xcms::featureChromatograms(data,
                                       features = feature_table$feature_id[sel_peaks],
                                       filled = TRUE)
  keep <- logical(length(sel_peaks))
  for(i in seq_along(sel_peaks)){
    keep[i] <- !MSnbase::isEmpty(chroms[i,])
  }

  dev_eic <- xcms::getEIC(xcms_set,
                          groupidx = sel_peaks,
                          rt = 'corrected')

  res <- list(sel_peaks = sel_peaks
              )
}
