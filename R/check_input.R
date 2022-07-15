#' @title Check proteomic data format
#' @description Check if input data.frame columns match one of the six allowed
#'  formats. Throws an error if an empty dataframe is used as input.
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
  if(ncol(df) == 0) {stop("Empty data frame passed to check_input")}
  
  # allowed formats: 4 variations
  allowed_formats = list(
    gene_rep = c('^gene$', '^rep[0-9]$'),
    gene_sample_control = c('^gene$', '^sample[0-9]$', '^control[0-9]$'),
    gene_signif = c('^gene$', '^logFC$', '^pvalue$', '^FDR$'),
    accession_rep = c('^accession_number$', 'rep[0-9]$'),
    accession_sample_control = c('^accession_number$', '^sample[0-9]$', '^control[0-9]$'),
    accession_signif = c('^accession_number$', '^logFC$', '^pvalue$', '^FDR$')
  )
  
  
  
  # check whether column is in allowed format using regex
  check = lapply(allowed_formats, function(fmt){
    mat = lapply(fmt, function(col_type) {
      grepl(col_type,colnames(df))
    })
    check_col_type_min = lapply(fmt, function(col_type) {
      col_type_bool <- grepl(col_type,colnames(df))
      check_col_type_min <- NULL
      if (col_type %in% c('^sample[0-9]$', '^control[0-9]$', '^rep[0-9]$')) {
        if (sum(col_type_bool) < 2) {
          check_col_type_min <- FALSE}}
      return(check_col_type_min)
    })
    vec = apply(do.call(rbind, mat), 2, any)
    # check that all col_type min count is satisfied, i.e.,
    #   all elements in check_col_type_min must be NULL, 
    #   if any one if FALSE, it does not satisfied the allowed format requirement
    check_all_col_type_min_count <- is.null(unlist(check_col_type_min))
    check_at_least_one_of_each <- all(unlist(lapply(mat, any)))
    check_min_allowed_columns <- length(vec[vec]) >= length(fmt)
    return(check_min_allowed_columns & 
             check_at_least_one_of_each & 
             check_all_col_type_min_count)
  })
  
  names(check) = names(allowed_formats)
  return(list(check=check, allowed=allowed_formats)) 
}
