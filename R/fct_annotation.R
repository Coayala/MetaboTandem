#' Load MS1 database
#'
#' @description Function to load MS1 databases
#' @param database Database name
#'
#' @return Dataframe with compound information
#'
#' @noRd
load_ms1_db <- function(database){
  if(database == 'massbank'){
    qr <- AnnotationHub::query(AnnotationHub::AnnotationHub(), "MassBank")

    cdb <- qr[["AH116166"]]
  } else {
    db_file <- switch(database,
                      mona = 'Z:/PhD_stuff/metabotandem_dbs/dbs/mona.sqlite',
                      hmdb_exp = 'Z:/PhD_stuff/metabotandem_dbs/dbs/hmdb_exp.sqlite',
                      hmdb_pred = 'Z:/PhD_stuff/metabotandem_dbs/dbs/hmdb_pred.sqlite',
                      gnps = 'Z:/PhD_stuff/metabotandem_dbs/dbs/gnps.sqlite')

    cdb <- CompoundDb::CompDb(db_file)
  }

  if(database == 'mona'){
    compounds_df <- CompoundDb::compounds(cdb,
                                          columns = c('compound_id', "name",
                                                      "original_spectrum_id",
                                                      "formula", "exactmass",
                                                      "smiles", "inchi",
                                                      "inchikey")) %>%
      dplyr::mutate(compound_id = original_spectrum_id) %>%
      dplyr::select(-original_spectrum_id)
  } else {
    compounds_df <- CompoundDb::compounds(cdb,
                                          columns = c("compound_id", "name",
                                                      "formula", "exactmass",
                                                      "smiles", "inchi",
                                                      "inchikey"))
  }

  compounds_df <- compounds_df %>%
    dplyr::mutate(exactmass = round(exactmass, digits = 6)) %>%
    dplyr::distinct()
}

#' Load MS2 database
#'
#' @description Function to load MS1 databases
#' @param database Database name
#'
#' @return Database spectra
#'
#' @noRd
load_ms2_db <- function(database){
  if(database == 'massbank'){
    qr <- AnnotationHub::query(AnnotationHub::AnnotationHub(), "MassBank")

    cdb <- qr[["AH116166"]]
  } else {
    db_file <- switch(database,
                      mona = 'Z:/PhD_stuff/metabotandem_dbs/dbs/mona.sqlite',
                      hmdb_exp = 'Z:/PhD_stuff/metabotandem_dbs/dbs/hmdb_exp.sqlite',
                      hmdb_pred = 'Z:/PhD_stuff/metabotandem_dbs/dbs/hmdb_pred.sqlite',
                      gnps = 'Z:/PhD_stuff/metabotandem_dbs/dbs/gnps.sqlite')

    cdb <- CompoundDb::CompDb(db_file)
  }

  spectra <- Spectra::Spectra(cdb)
}

#' Load MS2 database
#'
#' @description Function to load MS1 databases
#' @param database Database name
#'
#' @return Database spectra
#'
#' @noRd
get_ms2_matches <- function(match_obj, database){
  if(database %in% c('massbank', 'mona')){
    df <- Spectra::spectraData(match_obj) %>%
      as.data.frame %>%
      dplyr::filter(polarity == target_polarity,
                    stringr::str_detect(target_instrument_type, 'LC'))
  } else {
    df <- Spectra::spectraData(match_obj) %>%
      as.data.frame %>%
      dplyr::filter(polarity == target_polarity)
  }

  df <- df %>%
    dplyr::select(feature_id, precursorMz, rtime, target_compound_id,
                  target_formula, target_name, target_exactmass,
                  target_precursorMz, any_of('target_adduct'),
                  target_inchi,
                  target_inchikey, target_smiles, ms2_score = score) %>%
    dplyr::distinct() %>%
    dplyr::mutate(annotation_from_MS2 = TRUE)

  if(is.null(df$target_adduct)){
    df <- df %>%
      dplyr::mutate(target_adduct = NA)
  }

  return(df)
}

#' Get annotation tables
#'
#' @description Function to annotate features using MS1 and MS2 information
#'
#' @param feature_table Table with features mz and rt values
#' @param feature_spectra Features spectra
#' @param selected_dbs Vector with names of the databases to be used
#' @param adducts Vector with the adducts to be used for MS1 annotation
#' @param tolerance Allowed differences in m/z
#' @param ppm Allowed differences in ppm
#' @param req_precursor Check precursor for MS2 annotation
#' @param distance_thres Distance threshold for annotation
#' @param candidates How many annotations to keep for each feature
#'
#' @return List with annotations from each of the databases
#'
get_annotation_tables <- function(feature_table,
                                  feature_spectra,
                                  selected_dbs = c('massbank',
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

  BiocParallel::register(BiocParallel::SerialParam())

  annot_list <- list()
  match_list <- list()

  for(db_name in selected_dbs){

    print(paste0('Annotation with: ', db_name))

    ms2_db <- load_ms2_db(db_name)

    params_ms2 <- MetaboAnnotation::CompareSpectraParam(
      requirePrecursor = req_precursor,
      ppm = ppm,
      tolerance = tolerance,
      THRESHFUN = function(x) which(x >= distance_thres)
    )

    ms2_comp <- MetaboAnnotation::matchSpectra(feature_spectra,
                                               ms2_db,
                                               params_ms2)

    ms2_df <- get_ms2_matches(ms2_comp, db_name)

    params_ms1 <- MetaboAnnotation::Mass2MzParam(
      adducts = adducts,
      tolerance = tolerance,
      ppm = ppm
    )

    ms1_db <- load_ms1_db(db_name)

    ms1_match <- MetaboAnnotation::matchValues(query = feature_table,
                                               target = ms1_db,
                                               params_ms1)

    ms1_df <- MetaboAnnotation::matchedData(ms1_match) %>%
      as.data.frame %>%
      dplyr::select(feature_id, mz, rtime, adduct, target_compound_id,
                    target_name, target_exactmass, target_formula,
                    target_smiles, target_inchi, target_inchikey,
                    ms1_score = score, ms1_ppm_error = ppm_error) %>%
      dplyr::mutate(target_compound_id = as.character(target_compound_id))

    ms1_ms2_match <- ms1_df %>%
      dplyr::select(-target_compound_id, -target_exactmass) %>%
      dplyr::distinct() %>%
      dplyr::inner_join(ms2_df %>% dplyr::select(-rtime)) %>%
      dplyr::group_by(feature_id) %>%
      dplyr::slice_min(order_by = abs(ms2_score),
                       n = candidates,
                       with_ties = FALSE,
                       na_rm = FALSE)  %>%
      dplyr::select(feature_id, mz, rtime, annotation_from_MS2,
                    target_compound_id, target_name, target_exactmass,
                    target_formula, adduct_ms1_annotation = adduct,
                    precursorMz, ms1_ppm_error, ms1_score, ms2_score,
                    target_inchi, target_inchikey, target_smiles,
                    target_precursorMz, adduct_ms2_annotation = target_adduct)

    extra_ms1 <- ms1_df %>%
      dplyr::filter(!(feature_id %in% ms1_ms2_match$feature_id)) %>%
      dplyr::group_by(feature_id) %>%
      dplyr::slice_min(order_by = abs(ms1_score),
                       n = candidates,
                       with_ties = FALSE,
                       na_rm = FALSE) %>%
      dplyr::mutate(precursorMz = NA,
                    target_precursorMZ = NA,
                    target_adduct = NA,
                    ms2_score = NA,
                    annotation_from_MS2 = FALSE) %>%
      dplyr::filter(abs(ms1_ppm_error) <= 10)

    annots <- rbind(ms1_ms2_match, extra_ms1) %>%
      dplyr::select(-mz, -rtime)

    annot_final <- feature_table %>%
      dplyr::select(feature_id, mz, rtime) %>%
      dplyr::left_join(annots) %>%
      dplyr::select(-precursorMz)

    match_list[[db_name]] <- ms2_comp
    annot_list[[db_name]] <- annot_final

  }

  res <- list(annot_tables = annot_list,
              ms2_matches = match_list)

  return(res)
}

#' Merge annotation tables
#'
#' @description Function to merge annotations from multiple databases
#' @param annotation_tables_list List with annotations to merge
#' @param candidates How many annotations to keep for each feature
#'
#' @return Dataframe with top annotation from all databases
#'
merge_annotation_tables <- function(annotation_tables_list,
                                    feature_table,
                                    candidates = 1){

  all_ms2 <- purrr::imap(annotation_tables_list, function(df, db){

    df %>%
      dplyr::filter(annotation_from_MS2 == TRUE) %>%
      dplyr::select(feature_id, mz, rtime, annotation_from_MS2,
                    target_compound_id, target_name, target_exactmass,
                    target_formula, adduct_ms1_annotation, ms1_ppm_error,
                    ms1_score, ms2_score, target_inchi, target_inchikey,
                    target_smiles, target_precursorMz,
                    adduct_ms2_annotation) %>%
      dplyr::mutate(database = db)

  }) %>% purrr::reduce(rbind)

  all_ms2_selected <- all_ms2 %>%
    dplyr::group_by(feature_id) %>%
    dplyr::slice_max(order_by = ms2_score,
                     n = candidates,
                     with_ties = FALSE)

  all_ms1 <- purrr::imap(annotation_tables_list, function(df, db){

    df %>%
      dplyr::filter(annotation_from_MS2 != TRUE) %>%
      dplyr::select(feature_id, mz, rtime, annotation_from_MS2,
                    target_compound_id, target_name, target_exactmass,
                    target_formula, adduct_ms1_annotation, ms1_ppm_error,
                    ms1_score, ms2_score, target_inchi, target_inchikey,
                    target_smiles, target_precursorMz,
                    adduct_ms2_annotation) %>%
      dplyr::mutate(database = db)

  }) %>% purrr::reduce(rbind)


  all_ms1_selected <- all_ms1 %>%
    dplyr::filter(!(feature_id %in% all_ms2_selected$feature_id)) %>%
    dplyr::group_by(feature_id) %>%
    dplyr::slice_min(order_by = abs(ms1_ppm_error),
                     n = candidates,
                     with_ties = FALSE)

  all_annots_merged <- rbind(all_ms2_selected,
                             all_ms1_selected) %>%
    dplyr::select(-mz, -rtime)

  ready_table <- feature_table %>%
    dplyr::select(feature_id, mz, rtime) %>%
    dplyr::left_join(all_annots_merged) %>%
    dplyr::mutate(
      annotation_from_MS2 = ifelse(is.na(annotation_from_MS2),
                                   FALSE, annotation_from_MS2)
    )
}


#' Plot spectra annotation mirror
#'
#' @description Function to make a mirror plot of the annotation of a feature
#' @param matches_list List with matches to plot
#'
#' @return Dataframe with top annotation from all databases
#'
plot_mirror <- function(matches_list,
                        merged_annotation_table,
                        feature_id){

  ann_idx <- which(merged_annotation_table$feature_id == feature_id)

  sel_db <- merged_annotation_table$database[ann_idx]
  sel_score <- merged_annotation_table$ms2_score[ann_idx]

  db_match <- matches_list[[sel_db]]

  ft_idx <- which(Spectra::spectraData(db_match@query@backend)$feature_id == feature_id)

  match_idx <- which(db_match@matches$query_idx == ft_idx &
                       db_match@matches$score == sel_score)

  sel_match <- MetaboAnnotation::filterMatches(
    db_match,
    MetaboAnnotation::SelectMatchesParam(
      index = match_idx
    )
  )

  MetaboAnnotation::plotSpectraMirror(sel_match[ft_idx],
                                      scalePeaks = TRUE,
                                      ppm = 10,
                                      matchCol = 'darkgreen',
                                      ylab = 'Scaled intensity')
}

#' Classify metabolites
#'
#' @description Function to classify annotated metabolites using Classyfire
#' @param annotation_table Dataframe with all annotations
#'
#' @return Dataframe with molecular classification
#'
get_molecular_class <- function(annotation_table){

  inchikeys <- annotation_table %>%
    dplyr::select(target_inchikey) %>%
    tidyr::drop_na() %>%
    dplyr::distinct() %>%
    dplyr::pull()

  class_list <- purrr::map(inchikeys, classyfireR::get_classification)

  names(class_list) <- inchikeys

  class_dfs <- purrr::imap(class_list, function(cl, ikey){

    if(!is.null(cl)){
      df <- classyfireR::classification(cl) %>%
        dplyr::filter(Level %in% c('kingdom', 'superclass', 'class',
                                   'subclass', 'level 5')) %>%
        dplyr::select(-CHEMONT) %>%
        dplyr::mutate(inchikey = ikey)
    } else {
      df <- data.frame(
        Level = 'kingdom',
        Classification = NA,
        inchikey = ikey
      )
    }

    return(df)

  })

  all_classes <- purrr::reduce(class_dfs, rbind) %>%
    tidyr::pivot_wider(names_from = 'Level', values_from = 'Classification') %>%
    dplyr::select(-kingdom) %>%
    tidyr::drop_na(superclass)

  return(all_classes)
}

#' sirius
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
annotate_with_sirius <- function(feature_spectra,
                                 output_prefix,
                                 cores) {

  # Exporting spectra
  spectra_var_map <- c(feature_id = 'TITLE',
                       MsBackendMgf::spectraVariableMapping(MsBackendMgf::MsBackendMgf()))



  print('Exporting mgf')
  Spectra::export(feature_spectra, MsBackendMgf::MsBackendMgf(),
                  file = paste0(getwd(), '/', output_prefix, '.mgf'),
                  mapping = spectra_var_map)

  # Annotate sirius
  system2('sirius',
          args = c('--input', paste0(getwd(), '/', output_prefix, '.mgf'),
                   '--output', paste0(getwd(), '/', output_prefix, '.sirius'),
                   '--cores', cores,
                   'formula',
                   'fingerprints',
                   'canopus',
                   'structures',
                   'write-summaries',
                   '--output', paste0(getwd(), '/', output_prefix, '_summary')))

}

#' Add CANOPUS classification
#'
#' @description Function to add molecular classes predicted by CANOPUS
#' @param annotation_table Dataframe with annotations
#'
#' @return Dataframe with molecular classification
#'
add_canopus <- function(annotation_table,
                        molclass_table,
                        canopus_summary_file){

  canopus_classes <- readr::read_tsv(canopus_summary_file)

  canopus_formulas <- canopus_classes %>%
    dplyr::mutate(FeatureID = stringr::str_extract(mappingFeatureId, 'FT[0-9]+')) %>%
    dplyr::filter(adduct == '[M + H]+') %>%
    dplyr::select(FeatureID, canopus_formula = molecularFormula)

  canopus_processed <- canopus_classes %>%
    dplyr::filter(adduct == '[M + H]+') %>%
    dplyr::mutate(FeatureID = stringr::str_extract(mappingFeatureId, 'FT[0-9]+')) %>%
    dplyr::select(FeatureID, target_formula = molecularFormula,
                  superclass = `ClassyFire#superclass`, class = `ClassyFire#class`,
                  subclass = `ClassyFire#subclass`, `level 5` = `ClassyFire#level 5`)

  # Checking canopus_results

  class_from_ms2 <- annotation_table %>%
    dplyr::filter(annotation_from_MS2) %>%
    dplyr::inner_join(molclass_table, by = c('target_inchikey' = 'inchikey')) %>%
    dplyr::left_join(canopus_formulas, by = c('feature_id' = 'FeatureID')) %>%
    dplyr::mutate(
      annotation_notes = dplyr::case_when(
        target_formula == canopus_formula ~ 'Annotation based in MS2 spectral library. Match with SIRIUS predicted formula',
        TRUE ~ 'Annotation based in MS2 spectral library'
      ),
      classification_notes = 'Classification based on Classyfire') %>%
    dplyr::select(-canopus_formula)


  class_from_ms2_canopus <- annotation_table %>%
    dplyr::filter(annotation_from_MS2) %>%
    dplyr::filter(!(feature_id %in% class_from_ms2$feature_id)) %>%
    dplyr::left_join(canopus_processed, by = c('feature_id' = 'FeatureID', 'target_formula')) %>%
    dplyr::mutate(
      annotation_notes = dplyr::case_when(
        !is.na(superclass) ~ 'Annotation based in MS2 spectral library. Match with SIRIUS predicted formula',
        TRUE ~ 'Annotation based in MS2 spectral library'
      ),
      classification_notes = dplyr::case_when(
        !is.na(superclass) ~ 'Classification based on CANOPUS',
        TRUE ~ 'No classification'
      ))

  class_from_ms1_canopus <- annotation_table %>%
    dplyr::filter(!annotation_from_MS2,
                  !is.na(ms1_score)) %>%
    dplyr::inner_join(canopus_processed, by = c('feature_id' = 'FeatureID', 'target_formula')) %>%
    dplyr::inner_join(molclass_table, by = c('target_inchikey' = 'inchikey', 'superclass',
                                          'class', 'subclass', 'level 5')) %>%
    dplyr::mutate(annotation_notes = 'Annotation based on m/z matches. Match with SIRIUS predicted formula',
                  classification_notes = 'Classification based on CANOPUS')

  class_from_ms1 <- annotation_table %>%
    dplyr::filter(!annotation_from_MS2,
                  !is.na(ms1_score)) %>%
    dplyr::filter(!(feature_id %in% class_from_ms1_canopus$feature_id)) %>%
    dplyr::inner_join(molclass_table, by = c('target_inchikey' = 'inchikey')) %>%
    dplyr::mutate(annotation_notes = 'Annotation based on m/z matches. Match with SIRIUS predicted formula',
                  classification_notes = 'Classification based on Classyfire')

  class_from_canopus_only <- annotation_table %>%
    dplyr::select(-target_formula) %>%
    dplyr::filter(!(feature_id %in% c(class_from_ms1_canopus$feature_id,
                                      class_from_ms1$feature_id,
                                      class_from_ms2$feature_id,
                                      class_from_ms2_canopus$feature_id))) %>%
    dplyr::mutate(dplyr::across(dplyr::contains('target'), ~ NA)) %>%
    dplyr::inner_join(canopus_processed, by = c('feature_id' = 'FeatureID')) %>%
    dplyr::mutate(annotation_notes = 'No database match. Formula predicted by SIRIUS',
                  classification_notes = 'Classification based on CANOPUS')

  class_ready <- rbind(class_from_ms2,
                       class_from_ms2_canopus,
                       class_from_ms1,
                       class_from_ms1_canopus,
                       class_from_canopus_only) %>%
    dplyr::select(-mz, -rtime)

  return(class_ready)
}
