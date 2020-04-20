#' @title Check input data format
#' @description Check if input data columns match one of the four allowed formats.
#' @param tabl a data.frame
#' @note An item in the approved format list can appear more than once
#' @return list of boolean vairables indicating if input match a format and allowed formats
#' @export
#' @examples
#' \dontrun{
#' tabl1 = data.frame(gene = letters,rep1=runif(26), rep2=rnorm(26))
#' tabl2 = data.frame(gene = letters,rep1=runif(26), rep2=rnorm(26), somethingelse=1:26)
#' check_input(tabl1)
#' }
#' 


check_input <- function(tabl){
  
  # allowed formats: 4 variations
  allowed_formats = list(
    gene_rep = c('gene', 'rep[0-9]'),
    accession_rep = c('accession_number', 'rep[0-9]'),
    gene_signif = c('gene', 'logFC', 'pvalue', 'FDR'),
    accession_signif = c('accession_number', 'logFC', 'pvalue', 'FDR')
  )
  
  # check whether column is in allowed format using regex
  check = lapply(allowed_formats, function(x){
    mat = lapply(x, function(y) grepl(y,colnames(tabl)))
    vec = apply(do.call(rbind, mat), 2, any)
    return(length(vec) >= length(x) & all(unlist(lapply(mat, any))))
  })
  
  # should we remove the remaining columns?
  names(check) = names(allowed_formats)
  return(list(check=check, allowed=allowed_formats)) 
}

