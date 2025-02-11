#' Extract abundance table
#'
#' @description Function to extract the abundance table
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @return The return value, if any, from executing the function.
#'
#' @noRd
extract_abundance_table <- function(data) {
  print('Extracting abundance tables')
  df <- xcms::featureValues(data) %>%
    as.data.frame() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~ ifelse(is.na(.x), 0, .x))) %>%
    tibble::rownames_to_column(var = 'FeatureID') %>%
    dplyr::rename_with(~stringr::str_remove(.x, '\\.mzML$|\\.mzXML'))

  return(df)
}
