#' @title Retrieve irefindex for a given bait
#' @description Use irefindex_table data to get iRefIndex interactors and non-interactors of bait. 
#' See \code{?irefindex_table} for more details about the data set.
#' @param bait string. name of bait protein
#' @param score numeric. confidence score (number of publications) cutoff.
#' @return data.frame containing gene and significant columns for all non-bait iRefIndex genes 
#' (significant=T for iRefIndex interactors of bait). NULL if bait not found in iRefIndex.
#' @export
#' @examples
#' \dontrun{
#' df1 <- get_irefindex_list('BCL2',score = 2)
#' }

get_irefindex_list <- function(bait, score = 1){
  dbDf <- NULL
  
  if (!is.numeric(score)) stop ('score must be a numeric greater than 0!')
  if (score < 1) stop ('score must be a numeric greater than 0!')
  
  dbGenes <- unique(c(irefindex_table$Gene1,irefindex_table$Gene2))
  
  if (bait %in% dbGenes) { 
    tempDf1 <- subset(irefindex_table, (Gene1==bait | Gene2==bait) & Score>=score)
    if (nrow(tempDf1) > 1){
      ints <- unique(c(tempDf1$Gene1,tempDf1$Gene2))
      dbDf <- data.frame(gene=dbGenes[dbGenes!=bait])
      dbDf$significant <- dbDf$gene %in% ints 
    }
  }
  
  return(dbDf)
}
