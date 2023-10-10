#' @title Retrieve Bioplex 3.0 interactors for a given bait
#' @description Use bioplex_table data to get bioplex interactors and non-interactors of bait. 
#' See \code{?bioplex_table} for more details about the data set.
#' @param bait string. name of bait protein
#' @param p numeric. Probability of the protein being an interactor with the bait. See \code{?bioplex_table}.
#' 
#' @return data.frame containing gene and significant columns for all non-bait bioplex genes 
#' (significant=T for bioplex 3.0 interactors of bait). NULL if bait not found in bioplex.
#' 
#' @export
#' @examples
#' \dontrun{
#' df1 <- get_bioplex_list('BCL2',p = 0.5)
#' }

get_bioplex_list <- function(bait, p = 0.9){
  dbDf <- NULL
  
  if (!is.numeric(p)) stop ('p must be a numeric entry in [0,1].')
  if (p < 0 | p > 1) stop ('p must be a numeric entry in [0,1].')
  
  dbGenes <- unique(c(bioplex_table$Gene1,bioplex_table$Gene2))
  
  if (bait %in% dbGenes) { 
    tempDf1 <- subset(bioplex_table, (Gene1==bait | Gene2==bait) & pInt >= p)
    if (nrow(tempDf1) > 0){
      ints <- unique(c(tempDf1$Gene1,tempDf1$Gene2))
      dbDf <- data.frame(gene=dbGenes[dbGenes!=bait])
      dbDf$significant <- dbDf$gene %in% ints 
    }
  }
  
  return(dbDf)
}
