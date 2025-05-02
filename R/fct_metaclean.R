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
