#' @title Map SNP lists to genes
#' @description Create gene list data.frame from input SNP lists
#' @param infile a data.frame or a file path of SNP lists (two columns with header: listName, SNP) 
#' @param genes vector of gene names (genes detected in proteomic data)
#' @return data.frame containing listName, gene and SNP columns (restricting to genes in input genes vector).
#' @import hash
#' @export

get_snp_lists <- function(infile, genes){
  
  # handle file path or data frame input
  if (is.null(dim(infile))) {
	snpDf <- read.table(infile,header=T,sep="\t")
  } else { snpDf <- infile }
 
  # informative errors
  stop_invalid_columns(snpDf, 'SNP-data', c('listName','SNP'))
  
  # SNP-to-gene mapping, restricted to gene names in genes vector 
  mapDf <- NULL
  for (g in genes) {
    snpInGene <- snpDf$SNP %in% genes_snps[[g]]

    if (sum(snpInGene) > 0) {
      mapDf <- rbind(mapDf, data.frame(listName=snpDf$listName[snpInGene],
	gene=g, SNP=snpDf$SNP[snpInGene]))
    }
  }
  
  return(mapDf)
}
