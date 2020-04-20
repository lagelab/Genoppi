#' @title Stop invalid columns
#' @description yields an informative error message
#' if the a data object doesnt contain the correct
#' or verified column names.
#' @param data a data.frame
#' @param prefix e.g. 'snpData'
#' @param need vector of strings needed in data
#' @export
#' 

stop_invalid_columns <- function(data, prefix, need){
  # check format of infile
  bool = need %in% colnames(data)
  miss = paste(need[!bool], collapse = ', ')
  message = paste(prefix, 'need data columns:', miss)
  if (any(!bool)) stop(message)
}




