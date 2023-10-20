#' @title Retrieve tissue-specific expression data 
#' @description Create gene list data.frame from input tissues that can be superimposed onto volcano plots
#' or used for statistical enrichment analysis.
#' @param tissue vector of tissue names (character) included in dataset below. 
#' @param table dataset name (One of \code{?gtex_rna_table}, \code{?gtex_brain_table}, 
#' \code{?gtex_protein_table}, \code{?hpa_rna_table}).
#' @examples
#' \dontrun{
#' example_data %>% 
#'   calc_mod_ttest() %>% 
#'   id_significant_proteins() %>% 
#'   plot_volcano_basic() %>% 
#'   plot_overlay(as.bait('BCL2')) %>%
#'   plot_overlay(list(brain = get_tissue_list("Brain - Cortex",table = gtex_rna))) %>%
#'   theme_volcano()
#' }
#' @export

get_tissue_list <- function(tissue, table = gtex_rna_table){
  
  if (!is.character(tissue)) stop('tissue input must be character')
  if (!tissue %in% table$tissue) stop(paste(tissue,'is not a valid tissue. Please see ?get_tissue_list for details.'))
  
  return(table[table$tissue %in% tissue, ])
}


#' @title deprecated. See ?get_tissue_list
#' @description deprecated. See ?get_tissue_list
#' @param tissue deprecated. See ?get_tissue_list
#' @param table deprecated. See ?get_tissue_list
#' @export
get_tissue_lists <- function(tissue, table = gtex_rna_table){
  
  warning("'get_tissue_lists' has been deprecated. Please use 'get_tissue_list' instead.")
  get_tissue_list(tissue, table)
}

