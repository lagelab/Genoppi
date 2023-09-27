#' @title Map UniProt human protein accession number to gene name
#' @description Add gene column to data.frame containing accesion_number column.
#' Isoforms (indicated by suffixes separated by . or -) are mapped to the same gene names.
#' @param df data.frame containing accession_number column
#' @return data.frame consisting of input df + new gene column
#' @export

map_gene_id <- function(df){
  
  if (!'accession_number' %in% colnames(df)) {
    stop('"accession_number" column not found in input.')
  }
  if ('gene' %in% colnames(df)) {
    warning('"gene" column found in input, would be replaced in output.')
    df <- dplyr::select(df,!gene)
  } 

  # strip isoform suffixes in accession_number for mapping
  df$accession_noIso <- sapply(strsplit(as.character(df$accession_number),'(\\-)|(\\.)'),'[',1)
  
  # map accession_number in df to gene (unmapped entries shown as NA)
  df <- dplyr::left_join(df,accession_gene_table,
    by=dplyr::join_by(accession_noIso==accession_number))

  # drop unneeded columns
  df <- dplyr::select(df,!c(accession_noIso,all_genes))
	
  return(df) 
}
