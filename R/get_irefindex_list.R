#' @title Retrieve irefindex for a given bait
#' @description Use irefindex_table data to get IRefIndex interactors and non-interactors of bait. 
#' See \code{?irefindex_table} for more details about the data set.
#' @param bait string. name of bait protein
#' @param n numeric. Minimum number of publications that this interaction has been described in.
#' @return data.frame containing gene and significant columns for all non-bait IRefIndex genes 
#' (significant=T for IRefIndex interactors of bait). NULL if bait not found in IRefIndex.
#' @export
#' @examples
#' \dontrun{
#' df1 <- get_irefindex_list('BCL2',n = 1)
#' }

get_irefindex_list <- function(bait, n=1){
  dbDf <- NULL
  
  if (!is.numeric(n)) stop ('n must be a numeric greater than 0!')
  if (n < 1) stop ('n must be a numeric greater than 0!')
  
  dbGenes <- unique(c(irefindex_table$Gene1,irefindex_table$Gene2))
  
  if (bait %in% dbGenes) { 
    tempDf1 <- subset(irefindex_table, (Gene1==bait | Gene2==bait) & Score.np.max >= n)
    if (nrow(tempDf1) > 1){
      ints <- unique(c(tempDf1$Gene1,tempDf1$Gene2))
      dbDf <- data.frame(gene=dbGenes[dbGenes!=bait])
      dbDf$significant <- dbDf$gene %in% ints 
    }
  }
  
  return(dbDf)
}
