#' @title Retreive GWAS catalog data for a given list of traits
#' @description Create gene list data.frame from input traits
#' @param traits vector of GWAS catalog traits. See \code{gwas_table$DISEASE.TRAIT}
#' @param genes vector of gene names (genes detected in proteomic data)
#' @return data.frame containing GWAS catalog info (PUBMEDID, DISEASE.TRAIT, SNP, P.VALUE, STUDY.ACCESSION) + MAPPED_GENE columns (restricting to traits and genes in input vectors) 
#' @import hash
#' @export

get_gwas_lists <- function(traits, genes){

  # extract all GWAS catalog entries matching the input traits
  gwasDf <- gwas_table[gwas_table$DISEASE.TRAIT %in% traits,]

  # SNP-to-gene mapping, restricted to gene names in genes vector
  mapDf <- NULL
  for (g in genes) {
    snpInGene <- gwasDf$SNP %in% genes_snps[[g]]

    if (sum(snpInGene) > 0) {
      mapDf <- rbind(mapDf,data.frame(gwasDf[snpInGene,],gene=g)) 
    }
  }

  return(mapDf)
}
