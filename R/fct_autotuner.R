#' Start autotuner
#'
#' This function to create Autotuner object and get peak signals
#'
#'
#' @param metadata Sample information data.frame.
#' @param group Grouping variable.
#' @param lag Chromatographic scan points to test if next point is significant
#' @param threshold How many times larger intensity need to be to
#'     be considered significant
#' @param influence Factor to scale the magnitude of a significant scan
#' @param plot Logical value of whether to create a plot of the signals
#'
#' @export
start_autotuner <- function(metadata,
                            group,
                            lag,
                            threshold,
                            influence,
                            plot = FALSE){

  print('Creating Autotuner Object')
  autotuner_obj <- Autotuner::createAutotuner(metadata$FileName,
                                              metadata,
                                              file_col = 'FileName',
                                              factorCol = group)


  print('Getting signals')
  signals <- lapply(Autotuner::getAutoIntensity(autotuner_obj),
                    Autotuner::ThresholdingAlgo, lag, threshold, influence)

  if(plot){
    plot <- Autotuner::plot_signals(autotuner_obj,
                            threshold,
                            ## index for which data files should be displayed
                            sample_index = 1:nrow(metadata),
                            signals = signals)
  }

  print('Isolating peaks')
  autotuner_obj <- Autotuner::isolatePeaks(autotuner_obj,
                                           returned_peaks = 20,
                                           signals = signals)

  return(autotuner_obj)
}

#' Extract parameters AutoTuner
#'
#' This function extracts the parameters estimated by AutoTuner
#'
#'
#' @param massThr Sample information data.frame.
#'
#' @export
extract_autotuner <- function(autotuner_obj, massThr){
  eicParamEsts <- Autotuner::EICparams(Autotuner = autotuner_obj,
                                       massThresh = massThr,
                                       verbose = FALSE,
                                       returnPpmPlots = FALSE,
                                       useGap = TRUE)

  params <- Autotuner::returnParams(eicParamEsts, autotuner_obj)

  return(params$eicParams)


}
