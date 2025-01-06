#' MetaboTandem
#'
#' @description A class generator function
#'
#' @noRd
MetaboTandem <- R6::R6Class(
  classname = 'MetaboTandem',
  public = list(
    metadata = NA,
    data = NA,
    load_metadata = function(metadata_file){
      self$metadata <- load_metadata(metadata_file)
      invisible(self)
    },
    load_spectra_data = function(datadir,
                                 format = 'mzML',
                                 mode = 'onDisk'){
      tryCatch({self$data = load_spectra_data(datadir, self$metadata, format, mode)},
               error = function(e){
                 message('Error:\n', e)
               },
               finally = {
                 invisible(self)
               })
    },
    centroid_check = function(transform = TRUE){
      tryCatch({self$data = centroid_check(self$data, transform = transform)},
               error = function(e){
                 message('Error:\n', e)
               },
               finally = {
                 invisible(self)
               })

      invisible(self)
    },
    apply_peak_picking = function(method,
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
                                  cores = 1){
      tryCatch({self$data = apply_peak_picking(self$data,
                                               method = method,
                                               ppm = ppm,
                                               p_width = p_width,
                                               snt = snt,
                                               noise = noise,
                                               prefilter = prefilter,
                                               mz_diff = mz_diff,
                                               bin = bin,
                                               fwhm = fwhm,
                                               sigma = sigma,
                                               max = max,
                                               steps = steps,
                                               cores = cores)},
               error = function(e){
                 message('Error:\n', e)
               },
               finally = {
                 invisible(self)
               })
    },
    apply_peak_refinement = function(expand_rt = 2,
                                     expand_mz = 0,
                                     ppm = 10,
                                     min_prop = 0.75){
      tryCatch({self$data = apply_peak_refinement(self$data,
                                                  expand_rt = expand_rt,
                                                  expand_mz = expand_mz,
                                                  ppm = ppm,
                                                  min_prop = min_prop)},
               error = function(e){
                 message('Error:\n', e)
               },
               finally = {
                 invisible(self)
               })
    }
  )
)
