#' @title Retrieve Bioplex 3.0 interactors for a given bait
#' @description Use bioplex_table data to get BioPlex interactors and non-interactors of bait. 
#' See \code{?bioplex_table} for more details about the data set.
#' @param bait string. name of bait protein
#' @param score numeric. confidence score (between 0 and 1) cutoff. See \code{?bioplex_table}.
#' @return data.frame containing gene and significant columns for all non-bait BioPlex genes 
#' (significant=T for BioPlex 3.0 interactors of bait). NULL if bait not found in BioPlex.
#' 
#' @export
#' @examples
#' \dontrun{
#' df1 <- get_bioplex_list('BCL2',score = 0.9)
#' }

get_bioplex_list <- function(bait, score = 0){
  dbDf <- NULL
  
  if (!is.numeric(score)) stop ('score must be a numeric entry in [0,1].')
  if (score < 0 | score > 1) stop ('score must be a numeric entry in [0,1].')
  
  dbGenes <- unique(c(bioplex_table$Gene1,bioplex_table$Gene2))
  
  if (bait %in% dbGenes) {
    tempDf1 <- subset(bioplex_table, (Gene1==bait | Gene2==bait))
    
    # keep interactions with at least one confidence score >= cutoff
    # Score: string containing up to 2 scores delimited by comma)
    scoreList <- strsplit(tempDf1$Score,',')
    keep <- unlist(lapply(scoreList,function(x) any(as.numeric(x)>=score)))
    tempDf1 <- subset(tempDf1,keep)
    # alternative (expand cell lines into separate rows):
    #tempDf1 <- tidyr::separate_rows(tempDf1,Score,CellLine,sep=',',convert=T)
    #tempDf1 <- subset(tempDf1,Score>=0.score)

    if (nrow(tempDf1) > 0){
      ints <- unique(c(tempDf1$Gene1,tempDf1$Gene2))
      dbDf <- data.frame(gene=dbGenes[dbGenes!=bait])
      dbDf$significant <- dbDf$gene %in% ints 
    }
  }
  
  return(dbDf)
}
