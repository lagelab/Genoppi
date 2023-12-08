#' @title Retrieve STRING interactors for a given bait
#' @description Use string_table data to get STRING interactors and non-interactors of bait.
#' See \code{?string_table} for more details about the data set.
#' @param bait string. name of bait protein
#' @param score numeric. confidence score (between 0 and 1) cutoff.
#' @return data.frame containing gene and significant columns for all non-bait STRING genes
#' (significant=T for STRING interactors of bait). NULL if bait not found in STRING.
#' 
#' @export
#' @examples
#' \dontrun{
#' df1 <- get_string_list('BCL2',score = 0.9)
#' }

get_string_list <- function(bait, score = 0){
  dbDf <- NULL

  if (!is.numeric(score)) stop ('score must be a numeric entry in [0,1].')
  if (score < 0 | score > 1) stop ('score must be a numeric entry in [0,1].')

  dbGenes <- unique(c(string_table$Gene1,string_table$Gene2))

  if (bait %in% dbGenes) {
    tempDf1 <- subset(string_table, (Gene1==bait | Gene2==bait) & Score>=score)
    if (nrow(tempDf1) > 0){
      ints <- unique(c(tempDf1$Gene1,tempDf1$Gene2))
      dbDf <- data.frame(gene=dbGenes[dbGenes!=bait])
      dbDf$significant <- dbDf$gene %in% ints
    }
  }

  return(dbDf)
}
