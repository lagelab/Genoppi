#' @title Retrieve HuRI interactors for a given bait
#' @description Use huri_table data to get huri interactors and non-interactors of bait.
#' See \code{?huri_table} for more details about the data set.
#' @param bait string. name of bait protein
#'
#' @return data.frame containing gene and significant columns for all non-bait bioplex genes 
#' (significant=T for huri interactors of bait). NULL if bait not found in huri.
#' 
#' @export
#' @examples
#' \dontrun{
#' df1 <- get_huri_list('BCL2')
#' }

get_huri_list <- function(bait){
  dbDf <- NULL
  dbGenes <- unique(c(huri_table$Gene1,huri_table$Gene2))

  if (bait %in% dbGenes) {
    tempDf1 <- subset(huri_table, (Gene1==bait | Gene2==bait))
    if (nrow(tempDf1) > 0){
      ints <- unique(c(tempDf1$Gene1,tempDf1$Gene2))
      dbDf <- data.frame(gene=dbGenes[dbGenes!=bait])
      dbDf$significant <- dbDf$gene %in% ints
    }
  }

  return(dbDf)
}
