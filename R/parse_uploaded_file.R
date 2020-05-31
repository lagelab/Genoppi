#' @title parse uploaded file
#' @description the following actions are performed sequentially
#' 1) call read_input to get the file, 2) check whether accession_number
#' column is present, and if so, map to gene id; 3) calculate moderated
#' t-test on the data
#' @param path a string, data path
#' @return a data.frame ready for further analysis in genoppi 
#' @note this is for sequential parsing of files in the multiple
#' file tab in shiny.
#' @export
#' @family shiny


parse_uploaded_file <- function(path){
  
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
    pulldown$data <- calc_mod_ttest(pulldown$data)
  }


  return(pulldown$data)
}


