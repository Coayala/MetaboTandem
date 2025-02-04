#' Load DataFrame
#'
#' Function to load metadata into the MetaboTandem object
#'
#' @title Load dataframe
#' @description Load dataframe
#' @param file Path to the dataframe file
#'
#' @return A dataframe
#'
#' @export
load_dataframe <- function(file){
  ext <- tools::file_ext(file)
  if(ext == 'csv'){
    df <- vroom::vroom(file, delim = ",")
  } else if(ext == 'tsv'){
    df <- vroom::vroom(file, delim = "\t")
  } else{
    stop("Invalid file; Please upload a .csv or .tsv file")
  }

  return(df)
}


#' Load Metadata
#'
#' Function to load metadata
#'
#' @title Load Metadata
#' @description Function will load metadata and check its format
#' @param metadata_file Path to the dataframe file
#'
#' @return A dataframe with metadata
#'
#' @export
load_metadata <- function(metadata_file){

  metadata <- load_dataframe(metadata_file)

  if(!('SampleID' %in% colnames(metadata))){
    stop('Metadata requires a column named "SampleID" with sample names')
  } else if(ncol(metadata) < 2){
    stop('Metadata file requires at least two columns: "SampleID", and at least
         one other column with sample information')
  } else {
    return(metadata)
  }
}

#' Load Spectra data
#' This function is intended to load the spectra data in mzML or mzXML format
#' using the xcms package
#' @title Load Spectra Data
#' @description Loads spectra data in mzML or mzXML format using the xcms package.
#' @param datadir Path to the directory containing the data files.
#' @param metadata Contains sample information. Each row represents a sample and columns contain metadata information.
#' @param format Specifies the format of the data files. Default is 'mzML'. Supported formats include 'mzML' and 'mzXML'.
#' @param mode Specifies the mode of operation. Default is 'onDisk'. Other modes depend on xcms package capabilities.
#' @return An object containing the loaded spectra data.
#' @export
load_spectra_data <- function(datadir,
                              metadata,
                              format = 'mzML',
                              mode = 'onDisk'){
  # Get list of mass spectra files
  ms_files <- list.files(datadir, full.names = TRUE,
                         pattern = paste0('.*', format))

  if(length(ms_files) < 1){
    stop('No files of the specified format found in this directory')
  }

  # Read data as an `OnDiskMSnExp` object from xcms
  data <- MSnbase::readMSData(ms_files,
                              pdata = new('NAnnotatedDataFrame', metadata),
                              mode = mode,
                              verbose = TRUE)
  return(data)
}

#' Check centroided
#' This function checks if data is centroided
#' @importFrom MSnbase pickPeaks
#' @importFrom MSnbase fData
#' @importFrom MSnbase smooth
#' @title Check Centroided
#' @description Checks if the data is in centroid mode.
#' @param data MSnExp-class object. Represents mass spectrometry data.
#' @param transform Logical. If TRUE, transforms the data into centroid mode using available transformation functions. Default is TRUE.
#' @return MSnExp-class object with data either confirmed as centroid or transformed into centroid mode.
#' @export
centroid_check <- function(data,
                           transform = TRUE){
  if(!is.null(data)){
    is.centroided <- unique(MSnbase::fData(data)$centroided)
    if(is.centroided){
      print('Data is centroided')
    } else{
      print('Data is not centroided')
      if(transform){
        print('Transforming data')
        data_cent <- data %>%
          MSnbase::smooth(method = "SavitzkyGolay") %>%
          MSnbase::pickPeaks()
      }
    }
  } else {
    stop('Data has not been loaded')
  }

  return(data_cent)
}
