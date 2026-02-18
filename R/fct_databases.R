#' Extract field from string
#'
#' @noRd
extract_field_from_string <- function(x, field, delimiter = " __ ") {
  res <- sub(paste0(".*?", field, "(.*?)(", delimiter, ".*|$)"), "\\1", x)
  res[res == x] <- NA_character_
  res
}

#' Extract compounds sdf
#'
#' @noRd
extract_compounds_sdf_mona <- function(x) {

  colmap <- c(id = "ID",
              name = "NAME",
              inchi = "INCHIKEY",
              inchikey = "INCHIKEY",
              formula = "FORMULA",
              exactmass = "EXACT MASS",
              synonyms = "SYNONYMS",
              smiles = NA
  )
  sep <- " __ "
  ## Fix missing NAME entries in LipidMaps (see issue #1)
  nms <- x[, colmap["name"]]
  res <- tibble::tibble(original_spectrum_id = x[, colmap["id"]],
                        name = nms,
                        inchi = x[, colmap["inchi"]],
                        inchikey = x[, colmap["inchikey"]],
                        formula = x[, colmap["formula"]],
                        exactmass = as.numeric(x[, colmap["exactmass"]]),
                        synonyms = strsplit(x[, colmap["synonyms"]], split = sep)

  )
  ## Eventually reduce and normalize.
  inchis <- extract_field_from_string(x[, "COMMENT"], "InChI")
  nona <- !is.na(inchis)
  inchis[nona] <- paste0("InChI", inchis[nona])
  res$inchi <- inchis
  smiles <- extract_field_from_string(x[, "COMMENT"], "SMILES") %>%
    stringr::str_remove('^=')
  res$smiles <- smiles
  res$compound_id <- sprintf(paste0('CMP', "%0", ceiling(log10(nrow(res) + 1)), "d"),
                             seq_len(nrow(res)))
  res <- res %>%
    dplyr::select(compound_id, dplyr::everything())
}

#' Extract spectra
#'
#' @noRd
extract_spectra_sdf_mona <- function(x) {
  n <- nrow(x)
  plrty <- rep(NA_integer_, n)
  plrty[grep("P", x[, "ION MODE"])] <- 1L
  plrty[grep("N", x[, "ION MODE"])] <- 0L
  mzint <- lapply(strsplit(x[, "MASS SPECTRAL PEAKS"], " | __ "),
                  function(z) matrix(as.numeric(z), ncol = 2, byrow = TRUE))
  res <- data.frame(original_spectrum_id = x[, "ID"],
                    polarity = plrty,
                    collision_energy = x[, "COLLISION ENERGY"],
                    predicted = NA,
                    splash = NA_character_,
                    instrument_type = x[, "INSTRUMENT TYPE"],
                    instrument = x[, "INSTRUMENT"],
                    precursor_mz = as.numeric(x[, "PRECURSOR M/Z"]),
                    precursor_type = x[, "PRECURSOR TYPE"],
                    spectrum_type = x[, "SPECTRUM TYPE"],
                    spectrum_id = seq_len(nrow(x)),
                    stringsAsFactors = FALSE)
  res$compound_id <- sprintf(paste0('CMP', "%0", ceiling(log10(nrow(res) + 1)), "d"),
                             seq_len(nrow(res)))
  res$mz <- lapply(mzint, function(z) z[, 1])
  res$intensity <- lapply(mzint, function(z) z[, 2])
  res <- res %>%
    dplyr::select(compound_id, dplyr::everything())
}

#' Create database file for the Human Metabolome Database
#'
#' @description Function to create a SQlite database file to be used with CompoundDb, for the HMDB
#'
#'
create_hmdb_db <- function(structure_file,
                           spectra_directory,
                           source_version,
                           source_date,
                           predicted = FALSE){

  hmdb_compounds <- readr::read_csv(structure_file)

  hmdb_spectra <- CompoundDb::msms_spectra_hmdb(spectra_directory)

  db_name <- ifelse(predicted, 'HMDB_predicted', 'HMDB_experimental')

  hmdb_meta <- CompoundDb::make_metadata(source = db_name,
                                         url = "http://www.hmdb.ca",
                                         source_version = source_version,
                                         source_date = source_date)

  hmdb_spectra_filt <- hmdb_spectra %>%
    dplyr::filter(compound_id %in% hmdb_compounds$compound_id)

  hmdb_compounds <- hmdb_compounds %>%
    dplyr::filter(compound_id %in% hmdb_spectra_filt$compound_id)

  hmdb_db_file <- CompoundDb::createCompDb(hmdb_compounds,
                                           metadata = hmdb_meta,
                                           msms_spectra = hmdb_spectra_filt)

  glue::glue('HMDB database file created in ./{hmdb_db_file}')
}

#' Create database file for the MassBank of North America Databasse
#'
#' @description Function to create a SQlite database file to be used with CompoundDb, for the MoNA
#'
#'
create_mona_db <- function(structure_file,
                           source_version,
                           source_date){

  mona_sdf <- ChemmineR::datablock2ma(
    ChemmineR::datablock(ChemmineR::read.SDFset(structure_file, skipErrors = nonStop))
  )

  mona_compounds <- extract_compounds_sdf_mona(mona_sdf)

  mona_spectra <- extract_spectra_sdf_mona(mona_sdf)

  metad <- CompoundDb::make_metadata(source = 'MoNA',
                                     url = "http://mona.fiehnlab.ucdavis.edu/",
                                     source_version = source_version,
                                     source_date = source_date)


  mona_db_file <- CompoundDb::createCompDb(mona_compounds,
                                           metadata = metad,
                                           msms_spectra = mona_spectra)

  glue::glue('MoNA database file created in ./{mona_db_file}')
}

#' Create database file for the MassBank of North America Databasse
#'
#' @description Function to create a SQlite database file to be used with CompoundDb, for the MoNA
#'
#'
create_gnps_db <- function(compounds_info_file,
                           spectra_mgf,
                           source_version,
                           source_date){

  gnps_compounds <- readr::read_csv(compounds_info_file) %>%
    dplyr::select(compound_id = spectrum_id,
                  name = Compund_Name,
                  inchi = INCHI,
                  inchikey = InChIKey_smiles,
                  exactmass = ExactMass,
                  smiles = Smiles,
                  dplyr::everything()) %>%
    dplyr::mutate(synonyms = NA)

  safe_GET <- purrr::safely(httr::GET)
  i <- 0
  formula <- gnps_compounds %>%
    dplyr::select(smiles) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(formula = purrr::map_chr(smiles, function(x){
      x <- utils::URLencode(x, reserved = TRUE)
      url <- paste0('https://structure.gnps2.org/formula?smiles=', x)
      r <- safe_GET(url)

      if(is.null(r$error)){
        content <- try(httr::content(r$result, 'parsed', type = 'text/plain', encoding = "UTF-8"))
      } else {
        content = NA
      }

      i <<- i +1
      print(c('Finished', i))

      return(content)

    }))

  formula <- formula %>%
    dplyr::mutate(formula = ifelse(stringr::str_detect(formula, 'message|Bad'), NA, formula))

  gnps_compounds <- gnps_compounds %>%
    dplyr::left_join(formula, by = 'smiles')

  metad <- CompoundDb::make_metadata(source = "GNPS",
                                     url = "https://zenodo.org/records/11193898",
                                     source_version = source_version,
                                     source_date = source_date,
                                     organism = NA_character_)

  sps <- Spectra::Spectra(spectra_mgf,
                          source = MsBackendMgf(),
                          nonStop = TRUE)

  gnps_db_file <- CompoundDb::createCompDb(gnps_compounds,
                                           metadata = metad)

  cdb <- CompoundDb::CompDb(gnps_db_file, flags = RSQLite::SQLITE_RW)

  gnps_idx <- gnps_compounds %>%
    dplyr::select(compound_id, name, formula, inchi, inchikey,
                  exactmass, smiles)

  sps_data <- Spectra::spectraData(sps) %>%
    as.data.frame() %>%
    dplyr::rename(compound_id = TITLE,
                  name = COMPOUND_NAME,
                  smiles = SMILES,
                  intrument_type = MS_MASS_ANALYZER,
                  adduct = ADDUCT,
                  ionization = MS_IONISATION) %>%
    dplyr::mutate(polarity = dplyr::case_when(ION_MODE == 'positive' ~ 1,
                                              ION_MODE == 'negative' ~ 0,
                                              TRUE ~ NA),
                  polarity = as.integer(polarity),
                  spectrum_id = as.integer(n():1)) %>%
    dplyr::select(-COLLISION_ENERGY,
                  -SPECTRUMID,
                  -MS_MANUFACTURER,
                  -MS_DISSOCIATION_METHOD,
                  -ION_MODE) %>%
    dplyr::left_join(gnps_idx)

  sps_data$mz <- as.list(sps@backend@spectraData$mz)
  len_mzs <- purrr::map_dbl(sps@backend@spectraData$mz, length)
  sps_data$intensity <- as.list(sps@backend@spectraData$intensity)
  len_int <- purrr::map_dbl(sps@backend@spectraData$intensity, length)

  sps_data_ready <- sps_data[len_mzs > 0,]

  sps_back <- Spectra::Spectra(sps_data_ready)

  cdb <- CompoundDb::insertSpectra(cdb, spectra = sps_back)

  glue::glue('GNPS database file created in ./{gnps_db_file}')
}


