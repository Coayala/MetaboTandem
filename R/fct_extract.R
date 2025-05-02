#' Extract abundance table
#'
#' @description Function to extract the abundance table
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @return The return value, if any, from executing the function.
#'
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

#' Extract feature definitions
#'
#' @description Function to extract the abundance table
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @return The return value, if any, from executing the function.
#'
extract_feature_definitions <- function(data){
  xcms::featureDefinitions(data) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'feature_id') %>%
    dplyr::select(feature_id, mz = mzmed, mzmin, mzmax,
                  rtime = rtmed, rtmin, rtmax)
}

#' Extract feature spectra
#'
#' @description Function to extract the abundance table
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @return The return value, if any, from executing the function.
#'
extract_feature_spectra <- function(data,
                                    rm_low_int = TRUE,
                                    min_peaks = 3){

  spc <- xcms::featureSpectra(data,
                              return.type = 'Spectra')

  maxTic <- function(x, ...) {
    tic <- vapply(x, function(z) sum(z[, "intensity"], na.rm = TRUE),
                  numeric(1))
    x[[which.max(tic)]]
  }

  spc <- Spectra::combineSpectra(
    spc,
    FUN = maxTic,
    f = spc$feature_id,
    p = spc$feature_id
  )

  if(rm_low_int){
    #' Define a function to remove low intensity peaks
    low_int <- function(x, ...) {
      x > max(x, na.rm = TRUE) * 0.05
    }

    spc <- Spectra::filterIntensity(spc,
                                    intensity = low_int)
  }

  spc <- spc[lengths(spc) >= min_peaks]

  spc

}

#' Extract feature chromatograms
#'
#' @description Function to extract the abundance table
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @return The return value, if any, from executing the function.
#'
extract_feature_chromatograms <- function(data,
                                          feature_table){

  chroms <- xcms::featureChromatograms(data,
                                       features = feature_table$feature_id,
                                       filled = xcms::hasFilledChromPeaks(data))

  return(chroms)
}
