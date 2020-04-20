#' @title Retreive pathway annotations for a list of genes
#' @description Look up pathway annotations for genes using data from HGNC, GO (MF, CC, BP), or MSigDB (H,C1-C7) database.
#' @param database string ("hgnc","mf","cc","bp","h","c1","c2","c3","c4","c5","c6", or "c7") 
#' @param genes vector of gene names (genes detected in proteomic data)
#' @return data.frame containing gene, pathway, (and GO.ID if appropriate) columns 
#' @export

get_pathways <- function(database, genes){

  pathDf <- NULL

  # HGNC gene group
  if (database=="hgnc") {
    pathDf <- hgnc_group_table[hgnc_group_table$Gene.symbol %in% genes,]
 
  # GO molecular function
  } else if (database=="mf") {
    pathDf <- goa_mf_table[goa_mf_table$Gene.symbol %in% genes,]

  # GO cellular component
  } else if (database=="cc") {
    pathDf <- goa_cc_table[goa_cc_table$Gene.symbol %in% genes,]
 
  # GO biological process
  } else if (database=="bp") {
    pathDf <- goa_bp_table[goa_bp_table$Gene.symbol %in% genes,]

  # MSigDB H: hallmark gene sets
  } else if (database=="h") {
    pathDf <- msigdb_h_table[msigdb_h_table$Gene.symbol %in% genes,]

  # MSigDB C1: positional gene sets
  } else if (database=="c1") {
    pathDf <- msigdb_c1_table[msigdb_c1_table$Gene.symbol %in% genes,]

  # MSigDB C2: curated gene sets
  } else if (database=="c2") {
    pathDf <- msigdb_c2_table[msigdb_c2_table$Gene.symbol %in% genes,]

  # MSigDB C3: regulatory target gene sets
  } else if (database=="c3") {
    pathDf <- msigdb_c3_table[msigdb_c3_table$Gene.symbol %in% genes,]
 
  # MSigDB C4: computational gene sets
  } else if (database=="c4") {
    pathDf <- msigdb_c4_table[msigdb_c4_table$Gene.symbol %in% genes,]
  
  # MSigDB C5: GO gene sets
  } else if (database=="c5") {
    pathDf <- msigdb_c5_table[msigdb_c5_table$Gene.symbol %in% genes,]
  
  # MSigDB C6: oncogenic signatures
  } else if (database=="c6") {
    pathDf <- msigdb_c6_table[msigdb_c6_table$Gene.symbol %in% genes,]
 
  # MSigDB C7: immunologic signatures
  } else if (database=="c7") {
    pathDf <- msigdb_c7_table[msigdb_c7_table$Gene.symbol %in% genes,]
  }
  

  if (nrow(pathDf)==0) { pathDf <- NULL }

  if (!is.null(pathDf)) {
    if (database=="mf" | database=="cc" | database=="bp") {
      names(pathDf) <- c("gene","GO.ID","pathway")  
    } else { names(pathDf) <- c("gene","pathway") }
  }

  return(pathDf)
}

