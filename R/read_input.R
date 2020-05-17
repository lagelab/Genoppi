#' @title Read in proteomic data from file
#' @description Read in file containing proteomic data as data.frame. Call check_input() to check data format.
#' @param filename path to input file.
#' @param sep the field character seperator, see \code{?read.table}.
#' @return list contaiing two objects, data and format. data: input data.frame. format: list returned by check_input(), see \code{?check_input}. 
#' @export
#' @examples
#' \dontrun{
#' df1 <- data.frame(gene=letters, rep1=rnorm(26), rep2=rnorm(26), rep3=rnorm(26))
#' tmp <- tempfile()
#' write.table(df1, tmp, quote=F, sep="\t", row.names=F)
#' input <- read_input(tmp, sep="\t")
#' } 
#'

read_input <- function(filename, sep = ""){
 
  # check input file exists
  stopifnot(file.exists(filename)) 
 
  # read in file as data.frame
  df <- read.table(filename, header = T, sep = sep)

  # check input format and where columns are
  check <- check_input(df)
  
  return(list(data=df, format=check))

}
