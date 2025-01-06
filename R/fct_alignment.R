#' @title Apply Peak alignment
#' @description Function to apply peak alignment
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
#' @param plot Logical. If TRUE, plots the adjusted retention time. Default is FALSE.
#' @return MSnExp-class object after alignment.
#' @export
apply_alignment <- function(data,
                            metadata,
                            method){
  # Defining peak density parameters


  return(data_aligned)
}
