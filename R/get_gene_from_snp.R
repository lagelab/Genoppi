#' @title get the genes that matches a vector of SNPs
#' @description Create gene list data.frame from input SNP lists
#' @param snp a vector or character contain snp names (rsIDs)
#' @param invert boolean. Inverts the output, so that a list of SNPs is outputted instead.
#' @return a list of genes (if invert is FALSE)
#' @import hash

get_gene_from_snp <- function(snp, invert = F){
  
  stopifnot(is.character(snp) | is.vector(snp))
  
  # get snp to gene mapping
  mapping = lapply(names(genes_snps), function(gene){
    snp_mapping = genes_snps[[gene]]
    snp_found = snp %in% snp_mapping
    if (any(snp_found)) return(snp[snp_found])
  })
  
  # name vetors
  names(mapping) = names(genes_snps)
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
