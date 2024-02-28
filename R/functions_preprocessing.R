#' Load Spectra data
#'
#' This function is intended to load the spectra data in mzML or mzXML format
#' using the xcms package.
#'
library(BiocParallel)
#' @param datadir Path to the directory with the data.
#' @param metadata Sample information data.frame.
#' @param format Format of the data.
#'
#' @export
load_spectra_data <- function(datadir,
                              metadata,
                              format = 'mzML',
                              mode = 'onDisk'){

  # Get list of mass spectra files
  ms_files <- list.files(datadir, full.names = TRUE,
                         pattern = paste0('*.', format))

  # Read data as an `OnDiskMSnExp` object from xcms
  data <- MSnbase::readMSData(ms_files,
                              pdata = new('NAnnotatedDataFrame', metadata),
                              mode = mode,
                              verbose = TRUE)

  return(data)
}


#' Check centroided
#'
#' This function checks if data is centroided
#'
#'
#' @param data An [MSnExp-class] object.
#' @param transform Transform the data into centroid mode
#' @importFrom MSnbase pickPeaks
#' @importFrom MSnbase fData
#' @importFrom MSnbase smooth
#'
#' @export
centroid_check <- function(data,
                           transform = TRUE){

  is.centroided <- unique(MSnbase::fData(data)$centroided)
  if(is.centroided){
    print('Data is centroided')
  } else{
    print('Data is not centroided')
    if(transform){
      print('Transforming data')
      data_cent <- data %>%
        MSnbase::smooth(method = "SavitzkyGolay") %>%
        MSnbase::pickPeaks()
    }
  }

  return(data_cent)
}

#' Test peak picking
#'
#' Function to test peak picking parameters on a data subset
#'
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param mz.range *mz* range (`numeric(2)`) to test peak picking parameters.
#' @param rt.range *rt* range (`numeric(2)`) to test peak picking parameters.
#' @param p.width  Minium and maximum allowed peak width
#' @param snt Minimum signal-to-noise threshold allowed
#' @param noise Noise threshold
#' @param prefilter Prefilter step cutoff (`c(k, I)`). Mass will be retained if
#'     they contain at least `k` peaks with intensity >= `I`
#'
#' @importFrom MSnbase filterRt
#' @importFrom MSnbase filterMz
#' @importFrom MSnbase pData<-
#'
#' @export
test_peak_picking <- function(data,
                              mz_range,
                              rt_range,
                              p_width,
                              snt,
                              noise,
                              prefilter = c(1, 100)){
  # Test peak picking parameters

  print('Starting setting up parameters')

  cwp <- xcms::CentWaveParam(
    peakwidth = p_width,
    snthresh = snt,
    noise = noise,
    prefilter = prefilter
  )

  data %>%
    MSnbase::filterRt(rt = rt_range) %>%
    MSnbase::filterMz(mz = mz_range) %>%
    MSnbase::chromatogram(., aggregationFun="max") %>%
    xcms::findChromPeaks(., param = cwp) %>%
    xcms::plot(., col = "indianred2",
         ylab="Intensity", xlab="Retention Time (sec)",
         font.lab=1, cex.lab=1, cex.axis=1, font.main=1, cex.main=1)

  print('Graph done')


}

#' Apply Peak Picking
#'
#' Function to apply peak picking parameters on a data subset
#'
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param p_width  Minium and maximum allowed peak width.
#' @param snt Minimum signal-to-noise threshold allowed.
#' @param noise Noise threshold.
#' @param prefilter Prefilter step cutoff (`c(k, I)`). Mass will be retained if
#'     they contain at least `k` peaks with intensity >= `I`.
#'
#' @export
BPPARAM <- SnowParam(workers = 4, type ="SOCK")  # for parallel processing

apply_peak_picking <- function(data,
                               method,
                               ppm = 25,
                               p_width = c(20, 50),
                               snt = 3,
                               noise = 1e6,
                               prefilter = c(1, 100),
                               mz_diff = 0.001,
                               bin = 0.1,
                               fwhm = 30,
                               sigma = 12.72,
                               max = 10,
                               steps = 2,
                               BPPARAM = SnowParam(workers = 4, type ="SOCK")){
  # Test peak picking parameters

  if(method == 'cw'){
    sel_param <- xcms::CentWaveParam(
      ppm = ppm,
      peakwidth = p_width,
      snthresh = snt,
      noise = noise,
      prefilter = prefilter,
      mzdiff = mz_diff
    )
  } else if(method == 'mq'){
    sel_param <- xcms::MassifquantParam(
      ppm = ppm,
      peakwidth = p_width,
      snthresh = snt,
      noise = noise,
      prefilter = prefilter,
      mzdiff = mz_diff
    )
  } else if(method == 'mf'){
    sel_param <- xcms::MatchedFilterParam(
      binSize = bin,
      fwhm = fwhm,
      sigma = sigma,
      max = max,
      steps = steps
    )
  } else {
    print('Please enter "cw" for the CentWave algorithm, "mf" for the MatchedFilter algorithm,
          or "mq" for the Massifquant algorithm')
    stop()
  }

  print('Starting peak picking')

  data <- xcms::findChromPeaks(data, param = sel_param, BPPARAM = BPPARAM)

  return(data)
}



#
# print(benchmark_result)

# # Assuming BPPARAM is already defined for parallel processing
# system.time({
#   parallel_result <- apply_peak_picking(data, method = "cw", BPPARAM = BPPARAM)
# })
# print(system.time(parallel_result))




#' Apply Peak Refinement
#'
#' Function to apply peak picking parameters on a data subset
#'
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param expand_rt
#'
#' @export
apply_peak_refinement <- function(data,
                                  metadata,
                                  expand_rt = 2,
                                  expand_mz = 0,
                                  ppm = 10,
                                  min_prop = 0.75){

  mpp <- xcms::MergeNeighboringPeaksParam(expandRt = expand_rt,
                                          expandMz = expand_mz,
                                          ppm = ppm,
                                          minProp = min_prop)

  data <- xcms::refineChromPeaks(data, mpp)

  return(data)

}


#' Apply Peak alignment
#'
#' Function to apply peak picking parameters on a data subset
#'
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param metadata Sample information data.frame.
#' @param min_frac Minimum fraction of samples within a sample group a peak must
#'     be present to be considered a peak group.
#' @param min_samples Minimum number of samples in at least one sample group a
#'     peak must be present to be considered a peak group.
#' @param bin Size of the overlapping *mz* slices
#' @param bw = Bandwidth of the smoothing kernel
#' @param group_by Vector with the same length of the samples with grouping
#'     information
#'
#' @export
apply_alignment <- function(data,
                            metadata,
                            min_frac,
                            min_samples,
                            bin = 0.25,
                            bw = 30,
                            group_by,
                            plot = FALSE){
  # Defining peak density parameters

  sample_groups <- dplyr::pull(metadata, group_by)

  pdp <- xcms::PeakDensityParam(sampleGroups = sample_groups,
                          bw = bw,
                          minFraction = min_frac,
                          minSamples = min_samples,
                          binSize = bin)

  # Defining peak grouping parameters
  pgp <- xcms::PeakGroupsParam(minFraction = min_frac)

  print('Starting alignment')

  ## a - Group peaks
  data_grouped <- xcms::groupChromPeaks(data, param = pdp)
  ## b - alignment
  data_aligned <- xcms::adjustRtime(data_grouped, param = pgp)

  print('Alignment done')

  if(plot == TRUE){
    color_vector <- create_col_vector(metadata, color_by = group_by)
    xcms::plotAdjustedRtime(data_aligned,
                      col = color_vector,
                      xlab="Retention Time (sec)",
                      font.lab=2,
                      cex.lab=2,
                      cex.axis=2,
                      font.main=2,
                      cex.main=2,
                      lwd=2)
    legend("topright",
           legend = unique(names(color_vector)),
           col = unique(color_vector), lty=1)
    print('Plot done')
  }

  return(data_aligned)
}


#' Apply Peak Correspondence
#'
#' Function to apply peak picking parameters on a data subset
#'
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @param metadata Sample information data.frame.
#' @param min_frac Minimum fraction of samples within a sample group a peak must
#'     be present to be considered a peak group.
#' @param min_samples Minimum number of samples in at least one sample group a
#'     peak must be present to be considered a peak group.
#' @param bin Size of the overlapping *mz* slices
#' @param bw = Bandwidth of the smoothing kernel
#' @param group_by Vector with the same length of the samples with grouping
#'     information
#'
#' @export
apply_correspondence <- function(data,
                                 metadata,
                                 min_frac,
                                 min_samples,
                                 bin = 0.25,
                                 bw = 30,
                                 group_by){
  # Defining peak density parameters

  print('Starting grouping data')
  sample_groups <- dplyr::pull(metadata, group_by)
  pdp <- xcms::PeakDensityParam(sampleGroups = sample_groups,
                          bw = bw,
                          minFraction = min_frac,
                          minSamples = min_samples,
                          binSize = bin)

  ## a - Group peaks
  data_grouped <- xcms::groupChromPeaks(data, param = pdp)

  return(data_grouped)

}

#' Apply Gap Filling
#'
#' Function to apply peak picking parameters on a data subset
#'
#'
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @import ggplot2
#'
#' @export
apply_gap_filling <- function(data){

  ## determine the number of missing values
  number_na_i = sum(is.na(xcms::featureValues(data)))

  ## a - define parameter
  fpp <- xcms::FillChromPeaksParam(ppm = 2, expandMz = 0.25)

  ## b - fill in
  data_gap_filled <- xcms::fillChromPeaks(data, param=fpp)

  ## remaining number of na values
  number_na_f = sum(is.na(xcms::featureValues(data_gap_filled)))
  print(paste('The number of gap filled peaks was', number_na_i - number_na_f))

  return(data_gap_filled)
}

