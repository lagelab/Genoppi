#' @title Retrieve Bioplex 3.0 interactors for a given bait
#' @description Use pcnet_table data to get PCNet interactors and non-interactors of bait.
#' See \code{?pcnet_table} for more details about the data set.
#' @param bait string. name of bait protein
#'
#' @return data.frame containing gene and significant columns for all non-bait PCNet genes
#' (significant=T for PCNet interactors of bait). NULL if bait not found in PCNet.
#' 
#' @export
#' @examples
#' \dontrun{
#' df1 <- get_pcnet_list('BCL2')
#' }

get_pcnet_list <- function(bait){
  dbDf <- NULL
  dbGenes <- unique(c(pcnet_table$Gene1,pcnet_table$Gene2))

  if (bait %in% dbGenes) {
    tempDf1 <- subset(pcnet_table, (Gene1==bait | Gene2==bait))
    if (nrow(tempDf1) > 0){
      ints <- unique(c(tempDf1$Gene1,tempDf1$Gene2))
      dbDf <- data.frame(gene=dbGenes[dbGenes!=bait])
      dbDf$significant <- dbDf$gene %in% ints 
    }
  }
  
  return(dbDf)
}
