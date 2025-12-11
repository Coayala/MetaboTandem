#' Normalize data
#'
#' Function to apply different normalization methods to the data
#'
#'
#' @param abun_table Matrix or data frame with the unnormalized peak intensities
#'               with peaks as rows and samples as columns
#' @param norm_method Normalization method to apply
#' @param transform_data Logical value of whether log transform data or not
#'
#' @rdname normalization
#' @export
apply_normalization <- function(abun_table,
                                norm_method,
                                log_transform = FALSE){

  abun_table <- abun_table %>%
    tibble::column_to_rownames(var = 'FeatureID')

  norm_abun_table <- switch(norm_method,
                            global = global_norm(abun_table,
                                                 transform_data = log_transform),
                            median = median_norm(abun_table,
                                                 transform_data = log_transform),
                            mean = mean_norm(abun_table,
                                             transform_data = log_transform),
                            max = max_norm(abun_table,
                                           transform_data = log_transform),
                            vsn = median_norm(abun_table),
                            cycloess = median_norm(abun_table),
                            none = abun_table)

  norm_abun_table <- norm_abun_table %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'FeatureID')

  return(norm_abun_table)

}


#' @rdname normalization
#' @export
global_norm <- function(abun_table, transform_data = TRUE){
  colsum <- colSums(abun_table, na.rm = TRUE)
  colsum.median <- median(colsum)
  norm.abun_table <- data.frame(matrix(nrow = nrow(abun_table),
                                       ncol = ncol(abun_table)))
  for(col in 1:ncol(abun_table)){
    norm.abun_table[,col] <- (abun_table[,col] / colsum[col]) * colsum.median
  }
  colnames(norm.abun_table) <- colnames(abun_table)
  rownames(norm.abun_table) <- rownames(abun_table)
  if(transform_data == TRUE){
    lambda <- min(abun_table[abun_table != 0])
    norm.abun_table <- log10(abun_table + sqrt(abun_table^2 + lambda))
  }

  return(norm.abun_table)
}


#' @rdname normalization
#' @export
median_norm <- function(abun_table, transform_data = TRUE){

  colmedian <- apply(abun_table, 2, FUN = median, na.rm = TRUE)
  colmedian.mean <- mean(colmedian)
  norm.abun_table <- data.frame(matrix(nrow = nrow(abun_table),
                                       ncol = ncol(abun_table)))
  for(col in 1:ncol(abun_table)){
    norm.abun_table[,col] <- (abun_table[,col] / colmedian[col]) * colmedian.mean
  }
  colnames(norm.abun_table) <- colnames(abun_table)
  rownames(norm.abun_table) <- rownames(abun_table)
  if(transform_data == TRUE){
    lambda <- min(abun_table[abun_table != 0])
    norm.abun_table <- log10(abun_table + sqrt(abun_table^2 + lambda))
  }
  return(norm.abun_table)
}

#' @rdname normalization
#' @export
mean_norm <- function(abun_table, transform_data = TRUE){

  colmean <- colMeans(abun_table, na.rm = TRUE)
  colmean.mean <- mean(colmean)
  norm.abun_table <- data.frame(matrix(nrow = nrow(abun_table),
                                       ncol = ncol(abun_table)))
  for(col in 1:ncol(abun_table)){
    norm.abun_table[,col] <- (abun_table[,col] / colmean[col]) * colmean.mean
  }
  colnames(norm.abun_table) <- colnames(abun_table)
  rownames(norm.abun_table) <- rownames(abun_table)
  if(transform_data == TRUE){
    lambda <- min(abun_table[abun_table != 0])
    norm.abun_table <- log10(abun_table + sqrt(abun_table^2 + lambda))
  }
  return(norm.abun_table)
}

#' @rdname normalization
#' @export
vsn_norm <- function(abun_table){

  norm.abun_table <- suppressMessages(vsn::justvsn(as.matrix(abun_table)))
  norm.abun_table <- as.data.frame(norm.abun_table)
  return(norm.abun_table)
}

#' @rdname normalization
#' @export
cycloess_norm <- function(abun_table){

  norm.abun_table <- log2(abun_table)
  norm.abun_table <- limma::normalizeCyclicLoess(norm.abun_table, method = 'fast')
  norm.abun_table <- as.data.frame(norm.abun_table)
  rownames(norm.abun_table) <- rownames(abun_table)
  return(norm.abun_table)
}

#' @rdname normalization
#' @export
max_norm <- function(abun_table, transform_data = TRUE){

  colmax <- apply(abun_table, 2, FUN = max, na.rm = TRUE)
  norm.abun_table <- data.frame(matrix(NA, nrow = nrow(abun_table), ncol = ncol(abun_table)))
  for(col in 1:ncol(abun_table)){
    norm.abun_table[,col] <- (abun_table[,col] / colmax[col])
  }
  colnames(norm.abun_table) <- colnames(abun_table)
  rownames(norm.abun_table) <- rownames(abun_table)
  if(transform_data == TRUE){
    lambda <- min(abun_table[abun_table != 0])
    norm.abun_table <- log10(abun_table + sqrt(abun_table^2 + lambda))
  }

  return(norm.abun_table)
}


