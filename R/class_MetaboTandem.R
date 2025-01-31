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
    get_groups = function(){
      colnames(self$metadata)[-which(colnames(self$metadata) == 'SampleID')]
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
                                  ...){
      tryCatch({self$data = apply_peak_picking(self$data,
                                               method = method,
                                               ...)},
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
    },
    apply_alignment = function(method,
                               ...){
      tryCatch({self$data = apply_alignment(self$data,
                                            self$metadata,
                                            method = method,
                                            ...)},
               error = function(e){
                 message('Error:\n', e)
               },
               finally = {
                 invisible(self)
               })
    },
    apply_correspondence = function(method,
                                    ...){
      tryCatch({self$data = apply_correspondence(self$data,
                                                 self$metadata,
                                                 method = method,
                                                 ...)},
               error = function(e){
                 message('Error:\n', e)
               },
               finally = {
                 invisible(self)
               })
    }
  )
)
