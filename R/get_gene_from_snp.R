#' @title get the genes that matches a vector of SNPs
#' @description Create gene list data.frame from input SNP lists
#' @details Use SNP-to-gene mapping data included in genes_snps_hash object to 
#' identify genes in LD with input SNPs. 
#' @param snp a vector or character contain snp names (rsIDs)
#' @param invert boolean. Inverts the output, so that a list of SNPs is outputted instead.
#' @return a list of genes (if invert is FALSE)
#' @import hash
#' @examples 
#' \dontrun{
#' get_gene_from_snp('rs12516') # BRCA1
#' }
#' @export

get_gene_from_snp <- function(snp, invert = F){
  
  # get snp to gene mapping
  mapping = lapply(names(genes_snps_hash), function(gene){
    snp_mapping = genes_snps_hash[[gene]]
    snp_found = snp %in% snp_mapping
    if (any(snp_found)) return(snp[snp_found])
  })
  
  # name vetors
  names(mapping) = names(genes_snps_hash)
  mapping = null_omit(mapping)
  
  # Order SNP so it matches the input
  if (!invert) {
    mapping = split(rep(names(mapping), lengths(mapping)), unlist(mapping))
    ordered = lapply(snp, function(x){ if (x %in% names(mapping)) {
        return(mapping[[x]]) } else { return(NA) }})
    names(ordered) = snp
    mapping = ordered
  }
  
  return(mapping)
  
}
