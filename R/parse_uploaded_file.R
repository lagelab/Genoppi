#' @title parse uploaded file
#' @description the following actions are performed sequentially 1) call
#'   read_input to get the file, 2) check whether accession_number column is
#'   present, and if so, map to gene id; 3) calculate moderated t-test on the
#'   data
#' @param path a string, data path
#' @param two_sample a boolean, run two-sample mod-ttest if true, run one-sample
#'   mod-ttest otherwise
#' @return a data.frame ready for further analysis in genoppi
#' @note this is for sequential parsing of files in the multiple file tab in
#'   shiny.
#' @export
#' @family shiny


parse_uploaded_file <- function(path, two_sample=""){
  
  if (length(path) > 1) stop('expected a string, found a vector!')
  if (!file.exists(path)) stop(paste(path,'does not exist!'))
  
  # perform gene mapping
  pulldown = read_input(path, sep = '\t')
  if (pulldown$format$check$accession_rep == TRUE |
      pulldown$format$check$accession_signif == TRUE){
    pulldown$data <- map_gene_id(pulldown$data)
  }
  
  # perform moderated t-test
  if (pulldown$format$check$gene_rep |
      pulldown$format$check$accession_rep){
    two_sample_bool <- if (two_sample=="Two sample") T else if (two_sample=="One sample") F else NULL
    pulldown$data <- calc_mod_ttest(pulldown$data, two_sample=two_sample_bool)
  }


  return(pulldown$data)
}


