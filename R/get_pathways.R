#' @title Retreive pathway annotations for a list of genes
#' @description Look up pathway annotations for genes using data from HGNC, GO (MF,CC,BP), or MSigDB (H,C1-C8).
#' @param database string ('hgnc','mf','cc','bp','h','c1','c2','c3','c4','c5','c6','c7', or 'C8') 
#' @param genes vector of gene names
#' @return data.frame containing gene, pathway, (and GO.ID if appropriate) columns
#' @export

get_pathways <- function(database, genes){

  pathDf <- NULL
  
  if (!database %in% c('hgnc','mf','cc','bp','h','c1','c2','c3','c4','c5','c6','c7','c8')) {
    stop('invalid database input.')
  }

  # HGNC gene groups
  if (database=='hgnc') {
    pathDf <- subset(hgnc_group_table,Gene.symbol %in% genes)[,c('Gene.symbol','Group.name')]
 
  # GO terms
  } else if (database %in% c('mf','cc','bp')) {
    pathDf <- subset(get(paste('goa',database,'table',sep='_')),Gene.symbol %in% genes)[,c('Gene.symbol','GO.name')]

  # MSigDB collections
  } else if (database %in% c('h',paste('c',1:8,sep=''))) {
    pathDf <- subset(get(paste('msigdb',database,'table',sep='_')),Gene.symbol %in% genes)
  }

  if (nrow(pathDf)==0) {
    pathDf <- NULL # input genes not found in any pathways
  } else {
    names(pathDf) <- c('gene','pathway')
  }

  return(pathDf)
}

