#' @title Map Uniprot accession ID to gene name (HGNC symbol)
#' @description Add gene column to data.frame containing accesion_number column. Isoforms (indicated by suffixes separated by . or - in acession_number) are mapped to the same gene names.
#' @param df data.frame containing accession_number column
#' @return data.frame consisting of input df + new gene column
#' @export

map_gene_id <- function(df){
  
  stopifnot('accession_number' %in% colnames(df))
  
  # strip any isoform suffixes (separated by . or -) before mapping
  accession_noIsoform <- sapply(strsplit(as.character(df$accession_number),'(\\-)|(\\.)'),'[',1)
 
  # map accession_number in df to gene (unmapped entries shown as NA)
  matchInds <- match(accession_noIsoform,accession_gene_table$accession_number)
  df$gene <- as.factor(accession_gene_table$gene[matchInds])
 
  return(df) 
}
