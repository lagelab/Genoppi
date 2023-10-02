#' @title Retreive pathway annotations for a list of genes
#' @description Look up pathway annotations for genes using data from HGNC, GO (MF, CC, BP), or MSigDB (H,C1-C7) database.
#' @param database string ("hgnc","mf","cc","bp","h","c1","c2","c3","c4","c5","c6", or "c7") 
#' @param genes vector of gene names (genes detected in proteomic data)
#' @return data.frame containing gene, pathway, (and GO.ID if appropriate) columns
#' @export

get_pathways <- function(database, genes){

  pathDf <- NULL
  
  stopifnot(any(database %in% c("hgnc","mf","cc","bp","h","c1","c2","c3","c4","c5","c6","c7")))

  # HGNC gene group
  if (database=="hgnc") {
    pathDf <- subset(hgnc_group_table,Gene.symbol %in% genes)[,c('Gene.symbol','Group.name')]
 
  # GO molecular function
  } else if (database=="mf") {
    pathDf <- subset(goa_mf_table,Gene.symbol %in% genes)[,c('Gene.symbol','GO.name')]

  # GO cellular component
  } else if (database=="cc") {
    pathDf <- subset(goa_cc_table,Gene.symbol %in% genes)[,c('Gene.symbol','GO.name')]
 
  # GO biological process
  } else if (database=="bp") {
    pathDf <- subset(goa_bp_table,Gene.symbol %in% genes)[,c('Gene.symbol','GO.name')]

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
  

  if (nrow(pathDf)==0) {
    pathDf <- NULL
  } else {
    names(pathDf) <- c("gene","pathway")
  }

  return(pathDf)
}

