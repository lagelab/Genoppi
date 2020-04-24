#' @title parse uploaded file
#' @description the following actions are performed sequentially
#' 1) call read_input to get the file, 2) check whether accession_number
#' column is present, and if so, map to gene id; 3) calculate moderated
#' t-test on the data
#' @param data a string, data path
#' @return a data.frame ready for further analysis in genoppi 
#' @export
#' @family shiny


parse_uploaded_file <- function(path){
  
  if (length(path) > 1) stop('expected a strin, found a vector!')
  if (!file.exists(path)) stop(paste(path,'does not exist!'))
  
  pulldown = read_input(path, sep = '\t')
  if (pulldown$format$check$accession_rep == TRUE){
    pulldown$data <- map_gene_id(pulldown$data)
  }
  pulldown <- calc_mod_ttest(pulldown$data)
  return(pulldown)
  
}


