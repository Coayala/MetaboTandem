#' Test peak picking
#' Function to test peak picking parameters on a data subset
#' @title Test Peak Picking
#' @description Tests peak picking parameters on a subset of the data.
#' @return Modified MSnExp-class object with tested peak picking parameters.
#' @param ... Parameters to pass to the selected method
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param mz.range *mz* range (`numeric(2)`) to test peak picking parameters.
#' @param rt.range *rt* range (`numeric(2)`) to test peak picking parameters.
#' @param p.width  Minium and maximum allowed peak width
#' @param snt Minimum signal-to-noise threshold allowed
#' @param noise Noise threshold
#' @param prefilter Prefilter step cutoff (`c(k, I)`). Mass will be retained if they contain at least `k` peaks with intensity >= `I`
#' @importFrom MSnbase filterRt
#' @importFrom MSnbase filterMz
#' @importFrom MSnbase pData<-
#' @export
test_peak_picking <- function(data,
                              mz_range,
                              rt_range,
                              p_width,
                              snt,
                              noise,
                              prefilter = c(1, 100),
                              cores = 1){
  # Test peak picking parameters
  print('Starting setting up parameters')
  cwp <- xcms::CentWaveParam(
    peakwidth = p_width,
    snthresh = snt,
    noise = noise,
    prefilter = prefilter
  )


  if(cores == 1){
    data %>%
      MSnbase::filterRt(rt = rt_range) %>%
      MSnbase::filterMz(mz = mz_range) %>%
      MSnbase::chromatogram(., aggregationFun="max") %>%
      xcms::findChromPeaks(., param = cwp) %>%
      xcms::plot(., col = "indianred2",
                 ylab="Intensity", xlab="Retention Time (sec)",
                 font.lab=1, cex.lab=1, cex.axis=1, font.main=1, cex.main=1)
  } else if(cores > 1){

    print('Requesting multiple cores')

    BPPARAM <- BiocParallel::SnowParam(workers = cores)

    data %>%
      MSnbase::filterRt(rt = rt_range) %>%
      MSnbase::filterMz(mz = mz_range) %>%
      MSnbase::chromatogram(., aggregationFun="max") %>%
      xcms::findChromPeaks(., param = cwp, BBPARAM = BBPARAM) %>%
      xcms::plot(., col = "indianred2",
                 ylab="Intensity", xlab="Retention Time (sec)",
                 font.lab=1, cex.lab=1, cex.axis=1, font.main=1, cex.main=1)
  }
  print('Graph done')
}

#' Apply Peak Picking
#' Function to apply peak picking parameters on a data subset
#' @title Apply Peak Picking
#' @description Functions to perform peak picking on the data:
#'
#'  * `apply_peak_picking`
#'    * Wrapper to perform peak picking with any of the methods
#'  * `apply_peak_picking.cw`
#'    * Peak picking with the CentWave method
#'  * `apply_peak_picking.mf`
#'    * Peak picking with the Massifquant method
#'  * `apply_peak_picking.mq`
#'    * Peak picking with the MatchedFilter method
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param p_width  Minium and maximum allowed peak width.
#' @param snt Minimum signal-to-noise threshold allowed.
#' @param noise Noise threshold.
#' @param prefilter Prefilter step cutoff (`c(k, I)`). Mass will be retained if they contain at least `k` peaks with intensity >= `I`.
#' @param method Character. Specifies the peak picking method. Options include 'cw' for CentWave, 'mq' for Massifquant, and 'mf' for MatchedFilter.
#' @param ppm Numeric. Specifies the parts per million for m/z tolerance. Additional parameters include p_width, snt, noise, prefilter, mz_diff, bin, fwhm, sigma, max, and steps, each with specific roles in peak picking.
#' @param BPPARAM BiocParallelParam-class object. Specifies the parallel processing parameters. Default is SnowParam with 4 workers.
#' @return MSnExp-class object after applying peak picking.
#' @rdname peak_picking
#' @export
apply_peak_picking <- function(data,
                               method = c('cw', 'mf', 'mq'),
                               cores = 1,
                               ...){
  # Validating arguments
  method <- match.arg(method)

  # Validating number of cores
  if(cores == 1){
    print('Using a single core')

    BPPARAM <- BiocParallel::SerialParam()
  } else if(cores > 1){

    print('Requesting multiple cores')
    BPPARAM <- BiocParallel::SnowParam(workers = cores)
  }

  # Apply peak picking


  # Choosing methods
  if(method == 'cw'){

    data <- apply_peak_picking.cw(data, BPPARAM, ...)

  } else if(method == 'mq'){

    data <- apply_peak_picking.mq(data, BPPARAM, ...)

  } else if(method == 'mf'){

    data <- apply_peak_picking.mf(data, BPPARAM, ...)

  }

  return(data)
}

#' @rdname peak_picking
#' @export
apply_peak_picking.cw <- function(data,
                                  BPPARAM,
                                  ppm = 25,
                                  p_width = c(20, 50),
                                  snt = 3,
                                  noise = 1e6,
                                  prefilter = c(1, 100),
                                  mz_diff = 0.001,
                                  ...){

  # Create parameters object
  sel_param <- xcms::CentWaveParam(
    ppm = ppm,
    peakwidth = p_width,
    snthresh = snt,
    noise = noise,
    prefilter = prefilter,
    mzdiff = mz_diff
  )

  print('Starting peak picking with CentWave method')

  # Apply peak picking
  data <- xcms::findChromPeaks(data, param = sel_param, BPPARAM = BPPARAM)

  return(data)

}

#' @rdname peak_picking
#' @export
apply_peak_picking.mq <- function(data,
                                  BPPARAM,
                                  ppm = 25,
                                  p_width = c(20, 50),
                                  snt = 3,
                                  noise = 1e6,
                                  prefilter = c(1, 100),
                                  mz_diff = 0.001,
                                  ...){

  # Create parameters object
  params <- xcms::MassifquantParam(
    ppm = ppm,
    peakwidth = p_width,
    snthresh = snt,
    noise = noise,
    prefilter = prefilter,
    mzdiff = mz_diff
  )

  print('Starting peak picking with Massifquant method')

  # Apply peak picking
  data <- xcms::findChromPeaks(data, param = sel_param, BPPARAM = BPPARAM)

  return(data)
}

#' @rdname peak_picking
#' @export
apply_peak_picking.mf <- function(data,
                                  BPPARAM,
                                  bin = 0.1,
                                  fwhm = 30,
                                  sigma = 12.72,
                                  max = 10,
                                  steps = 2,
                                  ...){

  # Create parameters object
  sel_param <- xcms::MatchedFilterParam(
    binSize = bin,
    fwhm = fwhm,
    sigma = sigma,
    max = max,
    steps = steps
  )

  print('Starting peak picking with MatchedFilter method')

  # Apply peak picking
  data <- xcms::findChromPeaks(data, param = sel_param, BPPARAM = BPPARAM)

  return(data)

}

#' Apply Peak Refinement
#' Function to apply peak picking parameters on a data subset
#' @title Apply Peak Refinement
#' @description Function to apply peak picking parameters on a data subset.
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param expand_rt Numeric. Specifies the range to expand retention time during peak merging. Default is 2.
#' @param expand_mz Numeric. Specifies the range to expand m/z during peak merging. Default is 0.
#' @param ppm Numeric. Parts per million tolerance for peak merging.
#' @param min_prop Numeric. Minimum proportion of overlapping intensity required to merge peaks. Default is 0.75.
#' @return MSnExp-class object after refining peaks by merging neighboring peaks.
#' @export
apply_peak_refinement <- function(data,
                                  expand_rt = 2,
                                  expand_mz = 0,
                                  ppm = 10,
                                  min_prop = 0.75){

  # Create parameters object
  mpp <- xcms::MergeNeighboringPeaksParam(expandRt = expand_rt,
                                          expandMz = expand_mz,
                                          ppm = ppm,
                                          minProp = min_prop)

  print('Starting peak refining')

  BPPARAM <- BiocParallel::SerialParam()

  data <- xcms::refineChromPeaks(data, mpp, BPPARAM = BPPARAM)

  return(data)

}

