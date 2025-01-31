#' @title Apply Correspondence
#' @description Function to apply peak alignment
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param metadata Sample information data.frame.

#' @return MSnExp-class object after alignment.
#' @rdname correspondence
#' @export
apply_correspondence <- function(data,
                                 metadata,
                                 method = c('pd', 'np'),
                                 ...){

  # Validating arguments
  method <- match.arg(method)

  if(method == 'pd'){

    data <- apply_correspondence.pd(data, metadata, ...)

  } else if(method == 'np'){

    data <- apply_correspondence.np(data, metadata, ...)

  }

  return(data)
}

#' @rdname correspondence
#' @export
apply_correspondence.pd <- function(data,
                                    metadata,
                                    group_by,
                                    bandwidth = 30,
                                    bin_size = 0.25,
                                    ppm = 0,
                                    min_fraction = 0.9,
                                    max_features = 30,
                                    ...){

  # Creating parameter object
  groups <- metadata %>%
    dplyr::pull(group_by)

  group_params <- xcms::PeakDensityParam(
    sampleGroups = groups,
    bw = bandwidth,
    minFraction = min_fraction,
    minSamples = 1,
    binSize = bin_size,
    ppm = ppm,
    maxFeatures = max_features
  )

  # Apply correspondence
  data <- xcms::groupChromPeaks(data, param = group_params)

  return(data)

}

#' @rdname correspondence
#' @export
apply_correspondence.np <- function(data,
                                    metadata,
                                    group_by,
                                    mzvsrtbal = 10,
                                    abs_mz = 0.2,
                                    abs_rt = 15,
                                    knn = 10,
                                    ...){

  # Creating parameter object
  groups <- metadata %>%
    dplyr::pull(group_by)

  group_params <- xcms::NearestPeaksParam(
    sampleGroups = groups,
    mzVsRtBalance = mzvsrtbal,
    absMz = abs_mz,
    absRt = abs_rt,
    kNN = knn
  )

  # Apply correspondence
  data <- xcms::groupChromPeaks(data, param = group_params)

  return(data)

}
