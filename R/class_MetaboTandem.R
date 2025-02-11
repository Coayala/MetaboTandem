#' MetaboTandem
#'
#' @description A class generator function
#'
#' @noRd
MetaboTandem <- R6::R6Class(
  classname = 'MetaboTandem',
  public = list(
    metadata = NULL,
    data = NULL,
    abundance_table = NULL,
    norm_abundance_table = NULL,
    ordination = NULL,
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
    },
    apply_gap_filling = function(cores = 1){
      tryCatch({self$data = apply_gap_filling(self$data,
                                              cores = cores)},
               error = function(e){
                 message('Error:\n', e)
               },
               finally = {
                 invisible(self)
               })
    },
    extract_abundance_table = function(){
      self$abundance_table <- extract_abundance_table(self$data)
      invisible(self)
    },
    filter_and_normalize = function(min_perc_samples,
                                    filter_method, perc_remove,
                                    norm_method, log_transform){

      df <- apply_prevalence_filtering(
        self$abundance_table,
        min_perc_samples = min_perc_samples
      )

      df <- apply_variance_filtering(
        df,
        filter_method = filter_method,
        perc_remove = perc_remove
      )

      self$norm_abundance_table <- apply_normalization(
        df,
        norm_method = norm_method,
        log_transform = log_transform
      )

      invisible(self)

    },
    calculate_ordination = function(method,
                                    distance = NULL,
                                    group = NULL){
      self$ordination <- calculate_ordination(abun_table = self$norm_abundance_table,
                                              method,
                                              distance = distance,
                                              group = group)

      invisible(self)
    },
    plot_ordination = function(group_by,
                               add_sample_names = FALSE,
                               add_variables = FALSE,
                               add_ellipse = FALSE,
                               color_vector = NULL,
                               plot = TRUE){
      plot <- plot_ordination(ord_object = self$ordination,
                              metadata = self$metadata,
                              abundances = self$norm_abundance_table,
                              group_by,
                              add_sample_names,
                              add_variables,
                              add_ellipse,
                              color_vector,
                              plot)

      return(plot)
    }

  )
)
