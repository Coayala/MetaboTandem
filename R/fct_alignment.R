#' @title Apply Peak alignment
#' @description Function to apply peak alignment
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param metadata Sample information data.frame.

#' @return MSnExp-class object after alignment.
#' @rdname alignment
#' @export
apply_alignment <- function(data,
                            metadata,
                            method = c('pg', 'ow', 'lama'),
                            ...){

  # Validating arguments
  method <- match.arg(method)

  if(method == 'pg'){

    data <- apply_alignment.pg(data, metadata, ...)

  } else if(method == 'ow'){

    data <- apply_alignment.ow(data, metadata, ...)

  } else if(method == 'lama'){

    data <- apply_alignment.lama(data, metadata, ...)

  }

  return(data)
}

#' @rdname alignment
#' @export
apply_alignment.pg <- function(data,
                               metadata,
                               group_by,
                               bin_size = 0.25,
                               ppm_bin = 0,
                               min_fraction = 0.9,
                               extra_peaks = 1,
                               smooth = c('loess', 'linear'),
                               span = 1,
                               family = c('gaussian', 'symmetric'),
                               subset_samples = NULL,
                               subset_adjust = c('previous', 'average'),
                               ...){

  # Validating arguments
  smooth <- match.arg(smooth)
  family <- match.arg(family)
  subset_adjust <- match.arg(subset_adjust)

  # Creating parameters object
  if(!is.null(subset_samples)){
    subset_idx <- match(subset_samples, data@phenoData@data$SampleID)
  } else {
    subset_idx <- integer()
  }

  ## Parameters grouping

  groups <- metadata %>%
    dplyr::pull(group_by)

  group_params <- xcms::PeakDensityParam(
    sampleGroups = groups,
    bw = 30,
    minFraction = min_fraction,
    minSamples = 1,
    binSize = bin_size,
    ppm = ppm_bin,
    maxFeatures = 50
  )

  ## Parameters alignment
  sel_param <- xcms::PeakGroupsParam(
    minFraction = min_fraction,
    extraPeaks = extra_peaks,
    smooth = smooth,
    span = span,
    family = family,
    subset = subset_idx,
    subsetAdjust = subset_adjust
  )

  print('Starting alignment with Peak Groups method')

  if(xcms::hasAdjustedRtime(data)){
    data <- xcms::dropAdjustedRtime(data)
  }

  # Group peaks
  data <- xcms::groupChromPeaks(data, param = group_params)

  # Apply alignment
  data <- xcms::adjustRtime(data, param = sel_param)

  return(data)

}

#' @rdname alignment
#' @export
apply_alignment.ow <- function(data,
                               metadata,
                               bin_size = 1,
                               smooth_responsiveness = 1,
                               distance_function = c('cor', 'cor_opt', 'cov',
                                                     'prd', 'euc'),
                               local_alignment = FALSE,
                               subset_samples = NULL,
                               subset_adjust = c('previous', 'average'),
                               ...){

  # Validating arguments
  distance_function <- match.arg(distance_function)
  subset_adjust <- match.arg(subset_adjust)

  # Creating parameters object
  if(!is.null(subset_samples)){
    subset_idx <- sort(match(subset_samples, data@phenoData@data$SampleID))
  } else {
    subset_idx <- integer()
  }

  sel_param <- xcms::ObiwarpParam(
    binSize = bin_size,
    response = smooth_responsiveness,
    distFun = distance_function,
    localAlignment = local_alignment,
    subset = subset_idx,
    subsetAdjust = subset_adjust
  )


  print('Starting alignment with Obiwarp method')

  BiocParallel::register(BiocParallel::SerialParam())
  BiocParallel::bpparam('SerialParam')

  if(xcms::hasAdjustedRtime(data)){
    data <- xcms::dropAdjustedRtime(data)
  }

  # Apply alignment
  data <- xcms::adjustRtime(data, param = sel_param)

  return(data)

}

#' @rdname alignment
#' @export
apply_alignment.lama <- function(data,
                                 metadata,
                                 lama_file,
                                 lama_method = c('loess', 'gam'),
                                 span_lama = 0.5,
                                 ppm = 20,
                                 tolerance_mz = 0,
                                 tolerance_rt = 5,
                                 gam_smoothing =  c('tp', 'ts', 'ds', 'cr', 'cs',
                                                    'cc', 'sos', 'bs', 'ps', 're',
                                                    'mrf', 'gp', 'so'),
                                 ...){

  # Validating arguments
  lama_method <- match.arg(lam_method)
  gam_smoothing <- match.arg(gam_smoothing)

  lama_df <- load_dataframe(lama_file)
  if(ncol(lama_df) != 2 | !(colnames(lama_df) %in% c('mz', 'rt'))){
    stop('Landmarks file should have only two columns with "mz" and "rt"')
  }

  # Creating parameters object
  sel_param <- xcms::LamaParama(
    lamas = lama_df,
    method = lama_method,
    span = span_lama,
    ppm = ppm,
    tolerance = tolerance_mz,
    toleranceRt = tolerance_rt,
    bs = gam_smoothing
  )

  print('Starting alignment with Landmark method')

  if(xcms::hasAdjustedRtime(data)){
    data <- xcms::dropAdjustedRtime(data)
  }

  # Apply alignment
  data <- xcms::adjustRtime(data, param = sel_param)

  return(data)

}
