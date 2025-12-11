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
    diff_table = NULL,
    fitted_models = NULL,
    checked_models = NULL,
    contrasts_results = NULL,
    feature_definitions = NULL,
    feature_spectra = NULL,
    annotation_results = NULL,
    annotation_merged = NULL,
    classyfire_classes = NULL,
    canopus_classes = NULL,
    annotation_with_class = NULL,
    feature_chromatograms = NULL,
    autotuner_obj = NULL,
    load_metadata = function(metadata_file, autotuner = FALSE){
      self$metadata <- load_metadata(metadata_file,
                                     autotuner = autotuner)
      invisible(self)
    },
    get_groups = function(){
      colnames(self$metadata)[-which(colnames(self$metadata) == 'SampleID')]
    },
    load_spectra_data = function(datadir,
                                 format = 'mzML'){
      tryCatch({self$data = load_spectra_data(datadir, self$metadata, format)},
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
    apply_cleaning = function(group_by,
                              model_file){
      tryCatch({self$data = apply_cleaning(self$data,
                                           group_by = group_by,
                                           model_file = model_file)},
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
                                    filter_method,
                                    perc_remove,
                                    norm_method,
                                    log_transform){

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
    },
    calculate_permanova = function(vars,
                                   distance,
                                   assess = 'model',
                                   use_interaction = FALSE,
                                   strata = NULL){
      calculate_permanova(self$norm_abundance_table,
                          self$metadata,
                          vars = vars,
                          distance = distance,
                          assess = assess,
                          use_interaction = use_interaction,
                          strata = strata)
    },
    calculate_clustering = function(distance = 'euclidean',
                                    cluster_algorithm = 'ward.D2',
                                    add_kmeans = FALSE,
                                    k = 3,
                                    color_by = NULL,
                                    plot = TRUE,
                                    color_vector = NULL){
      calculate_clustering(self$norm_abundance_table,
                           self$metadata,
                           distance = distance,
                           cluster_algorithm = cluster_algorithm,
                           add_kmeans = add_kmeans,
                           k = k,
                           color_by = color_by,
                           plot = plot,
                           color_vector = color_vector)
    },
    differential_analysis = function(group,
                                     control_condition,
                                     treatment_condition){
      self$diff_table <- differential_analysis(self$abundance_table,
                                               self$metadata,
                                               norm_method = 'median',
                                               group = group,
                                               control_condition = control_condition,
                                               treatment_condition = treatment_condition)
      invisible(self)
    },
    plot_volcano = function(pval_thres,
                            log2fc_thres,
                            use_adjusted_pval = FALSE){
      plot_volcano(self$diff_table,
                   pval_thres = pval_thres,
                   log2fc_thres = log2fc_thres,
                   use_adjusted_pval = use_adjusted_pval)
    },
    plot_da_sig_features = function(pval_thres,
                                    log2fc_thres,
                                    color_by,
                                    use_adjusted_pval = FALSE,
                                    cluster_feat = FALSE,
                                    cluster_samp = FALSE,
                                    color_vector = NULL){
      plot_da_sig_features(self$norm_abundance_table,
                           self$metadata,
                           self$diff_table,
                           pval_thres = pval_thres,
                           log2fc_thres = log2fc_thres,
                           color_by = color_by,
                           use_adjusted_pval = use_adjusted_pval,
                           cluster_feat = cluster_feat,
                           cluster_samp = cluster_samp,
                           color_vector = color_vector)
    },
    fit_models = function(model_type,
                          vars = NULL,
                          fix_vars = NULL,
                          rand_vars = NULL){

      self$fitted_models <- fit_model(self$norm_abundance_table,
                                      self$metadata,
                                      model_type = model_type,
                                      vars = vars,
                                      fix_vars = fix_vars,
                                      rand_vars = rand_vars)

      invisible(self)

    },
    check_model = function(){
      self$checked_models <- check_model(self$fitted_models)

      invisible(self)
    },
    test_contrasts = function(L){
      self$contrasts_results <- test_contrasts(self$fitted_models,
                                               L = L)
      invisible(self)

    },
    plot_model_sig_features = function(color_by,
                                       cluster_feat = FALSE,
                                       cluster_samp = FALSE,
                                       color_vector = NULL){
      plot_model_sig_features(self$norm_abundance_table,
                              self$metadata,
                              self$contrasts_results,
                              color_by = color_by,
                              cluster_feat = cluster_feat,
                              cluster_samp = cluster_samp,
                              color_vector = color_vector)
    },
    extract_feature_definitions = function(){
      self$feature_definitions <- extract_feature_definitions(self$data)

      invisible(self)
    },
    extract_feature_spectra = function(rm_low_int = TRUE,
                                       min_peaks = 3){

      self$feature_spectra <- extract_feature_spectra(self$data,
                                                      rm_low_int = rm_low_int,
                                                      min_peaks = min_peaks)

      invisible(self)
    },
    extract_feature_chromatograms = function(){
      self$feature_chromatograms <- extract_feature_chromatograms(self$data,
                                                                  self$feature_definitions)

      invisible(self)
    },
    get_annotation_tables = function(selected_dbs = c('massbank',
                                                      'mona',
                                                      'hmdb_exp',
                                                      'hmdb_pred',
                                                      'gnps'),
                                     adducts = c("[M+H]+"),
                                     tolerance = 0.005,
                                     ppm = 5,
                                     req_precursor = TRUE,
                                     distance_thres = 0.5,
                                     candidates = 1){

      self$annotation_results <- get_annotation_tables(self$feature_definitions,
                                                       self$feature_spectra,
                                                       selected_dbs = selected_dbs,
                                                       adducts = adducts,
                                                       tolerance = tolerance,
                                                       ppm = ppm,
                                                       req_precursor = req_precursor,
                                                       distance_thres = distance_thres,
                                                       candidates = candidates)

      invisible(self)

    },
    merge_annotation_tables = function(candidates = 1){

      self$annotation_merged <- merge_annotation_tables(self$annotation_results$annot_tables,
                                                        self$feature_definitions,
                                                        candidates = candidates)

      invisible(self)
    },
    run_classyfire = function(){
      self$classyfire_classes <- run_classyfire(self$annotation_merged)

      invisible(self)
    },
    run_sirius = function(output_prefix,
                          cores){
      self$canopus_classes <- run_sirius(self$annotation_merged,
                                         output_prefix,
                                         cores)

      invisible(self)
    },
    add_molecular_classes = function(){
      self$annotation_with_class <- add_molecular_classes(self$annotation_merged,
                                                          self$classyfire_classes,
                                                          self$canopus_classes)

      invisible(self)
    },
    plot_mirror = function(feature_id){
      plot_mirror(self$annotation_results$ms2_matches,
                  self$annotation_merged,
                  feature_id = feature_id)
    },
    start_autotuner = function(group,
                               lag,
                               threshold,
                               influence,
                               plot = FALSE){

      self$autotuner_obj <- start_autotuner(self$metadata,
                                            group = group,
                                            lag = lag,
                                            threshold = threshold,
                                            influence = influence,
                                            plot = plot)

      invisible(self)

    },
    extract_autotuner = function(){
      extract_autotuner(self$autotuner_obj,
                        massThr = 0.005)
    }
  )
)
