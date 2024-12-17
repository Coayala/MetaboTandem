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
      self$data = load_spectra_data(datadir, self$metadata, format, mode)
      invisible(self)
    },
    centroid_check = function(transform = TRUE){
      self$data = centroid_check(self$data, transform = transform)
      invisible(self)
    }
  )
)
