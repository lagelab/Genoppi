#' @title Retrieve PPI data for a given bait
#' @description Get interactors and non-interactors of bait from specified PPI dataset.
#' @param bait name of bait protein
#' @param dataset one of "bioplex", "inweb", "irefindex", "string"
#' @details this function does not use confidence score cutoffs to filter interactions.
#' Use \code{?get_bioplex_list}, \code{?get_inweb_list}, \code{?get_irefindex_list}, and 
#' \code{?get_string_list} instead to set cutoffs.  
#' @return data.frame containing gene and significant columns for all non-bait genes in
#' specific dataset (significant=T for interactors of bait). NULL if bait not found in dataset.
#' @export
#' @examples
#' \dontrun{
#' df1 <- get_ppi_list('BCL2','inweb')
#' }

get_ppi_list <- function(bait, dataset){
  
  if (!dataset %in% c('bioplex','inweb','irefindex','string')) {
	stop('dataset must be "bioplex", "inweb", "irefindex", or "string"')}

  dataset_lookup_table <- list(
    'bioplex' = genoppi::get_bioplex_list,
    'inweb' = genoppi::get_inweb_list,
    'irefindex' = genoppi::get_irefindex_list,
    'string' = genoppi::get_string_list
  )
  dataset_func <- dataset_lookup_table[[as.character(dataset)]]

  return(dataset_func(bait))
}

