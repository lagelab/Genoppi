#' @title Read input data
#' @description Read in input file and store as data.frame. Call check_input() to check data format.
#' @param filename path to input file
#' @param sep the field character seperator, see \code{?fread}
#' @param header a logical value indicating whether the file contains the names of the variables as its first line
#' @return list contaiing input data.table and list of boolean variables indicating input format
#' @export

read_input <- function(filename, header = 'auto', sep = 'auto'){
  
  #input = read.table(filename, header = header, sep = sep)
  input = data.table::fread(filename, header = header, sep = sep)
  check = check_input(input)
  
  return(list(data=input, format=check))
}

