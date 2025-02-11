#' @title Apply Gap Filling
#' @description Function for gap filling
#' @param data An [MSnExp-class] object in *centroid* mode.
#' @return MSnExp-class object after alignment.
#' @export
apply_gap_filling <- function(data,
                              cores = 1) {

  if(cores == 1){
    print('Using a single core')

    BPPARAM <- BiocParallel::SerialParam()
  } else if(cores > 1){

    print('Requesting multiple cores')
    BPPARAM <- BiocParallel::SnowParam(workers = cores)
  }

  param <- xcms::ChromPeakAreaParam()

  data <- xcms::fillChromPeaks(data, param = param, BPPARAM = BPPARAM)


  return(data)
}
