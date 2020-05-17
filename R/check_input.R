#' @title Check proteomic data format
#' @description Check if input data.frame columns match one of the four allowed formats.
#' @param df data.frame containing proteomic data.
#' @note An item in the approved format list can appear more than once.
#' @return list of two lists: check and allowed. check: list of logical vairables indicating if input match an allowed format. allowed: a list of all allowed formats.
#' @export
#' @examples
#' \dontrun{
#' tabl1 = data.frame(gene = letters,rep1=runif(26), rep2=rnorm(26))
#' check_input(tabl1)
#' tabl2 = data.frame(gene = letters,rep1=runif(26), rep2=rnorm(26), somethingelse=1:26)
#' check_input(tabl2)
#' }
#' 

check_input <- function(df){

  # check input is data.frame
  stopifnot(is.data.frame(df))
  
  # allowed formats: 4 variations
  allowed_formats = list(
    gene_rep = c('gene', 'rep[0-9]'),
    accession_rep = c('accession_number', 'rep[0-9]'),
    gene_signif = c('gene', 'logFC', 'pvalue', 'FDR'),
    accession_signif = c('accession_number', 'logFC', 'pvalue', 'FDR')
  )
  
  # check whether column is in allowed format using regex
  check = lapply(allowed_formats, function(x){
    mat = lapply(x, function(y) grepl(y,colnames(df)))
    vec = apply(do.call(rbind, mat), 2, any)
    return(length(vec) >= length(x) & all(unlist(lapply(mat, any))))
  })
  
  names(check) = names(allowed_formats)
  return(list(check=check, allowed=allowed_formats)) 
}
