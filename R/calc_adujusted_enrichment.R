#' @title calculate adjusted enrichment
#' @description calculate hypergeometric enrichment of genes in tissue and 
#' subsequent adjusting for multiple testing.
#' @param data proteomic data with gene and significant columns.
#' @param reference table with col.by, gene, significant in columns.
#' @param col.by string. What column contains the group?
#' @param bait bait. String. Passed to \code{?calc_hyper}.
#' @param p.adj.method Default is fdr. See p.adjust.methods in \code{?stats::p.adjust}.
#' @param intersectN Boolean indicating if the total population should be the intersect
#' of the two datasets, when calculating hypergeometric overlap.
#' @examples
#' \dontrun{
#' # check for enrichment in GTEx
#' data(gtex_table)
#' data(example_data)
#' data = example_data %>% calc_mod_ttest %>% id_enriched_proteins()
#' gtex_enrichment = calc_adjusted_enrichment(data, gtex_table)
#' gtex_enrichment
#' }
#' @export

calc_adjusted_enrichment <- function(data, reference, col.by = 'tissue', bait = NULL, p.adj.method = 'fdr', intersectN = T){
  
  stopifnot(all(c('gene', 'significant') %in% colnames(data)))
  stopifnot(all(c('gene', 'significant', col.by) %in% colnames(reference)))
  
  byval = unique(reference[[col.by]])
  statistics = lapply(byval, function(x){
    
    # calculate hypergeometric enrichment
    query_list = data.frame(listName = x, reference[reference[[col.by]] == x, ])
    query_intersect = data.frame(listName = x , intersectN = intersectN)
    hyper = suppressWarnings(calc_hyper(data, query_list, query_intersect, bait = bait))
    sucess_genes = paste0(hyper$genes[[1]]$successInSample_genes, collapse = '; ')
    hyper$statistics$successInSampleGenes <- sucess_genes
    return(hyper$statistics)
    
  })
  
  # combine results and calculate adjusted p-values
  statistics <- do.call(rbind, statistics)
  statistics$BH.FDR <- stats::p.adjust(statistics$pvalue, method = p.adj.method)
  rank <- order(statistics$BH.FDR, statistics$pvalue)
  statistics <- statistics[rank,]
  statistics$list_name <- factor(statistics$list_name, levels = rev(statistics$list_name))
  
  return(statistics)
}
