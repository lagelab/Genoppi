#' @title Retrieve InWeb data for a given bait
#' @description Use inweb_table data to get InWeb interactors and non-interactors of bait.
#' @param bait name of bait protein
#' @param type string ("all","hc","gs") indicating types of interactors to return (all, high-confidence, or gold-standard)
#' 
#' @return data.frame containing gene and significant columns for all non-bait InWeb genes (significant=T for InWeb interactors of bait). NULL if bait not found in InWeb.
#' 
#' @export
#' @examples
#' \dontrun{
#' df1 <- get_inweb_list('BCL2',type='all')
#' df2 <- get_inweb_list('BCL2',type='hc')
#' df3 <- get_inweb_list('BCL2',type='gs')
#' }

get_inweb_list <- function(bait, type="all"){
  inwebDf <- NULL

  if (!any(type %in% c("all","hc","gs"))) stop ('type must be "all","gs", or "hc"')
 
  inwebGenes <- unique(c(inweb_table$Gene1,inweb_table$Gene2))

  if (bait %in% inwebGenes) { 
    # get all interactions
    if (type=="all") {
      tempDf <- subset(inweb_table, Gene1==bait | Gene2==bait)
      ints <- unique(c(tempDf$Gene1,tempDf$Gene2))
    
    # get high-confidence interactions (InWeb3 ints with score > 0.154, + all InWeb_IM ints)
    } else if (type=="hc") {
      tempDf1 <- subset(inweb_table, (Gene1==bait | Gene2==bait) & Source=="InWeb3" & Score>0.154)
      tempDf2 <- subset(inweb_table, (Gene1==bait | Gene2==bait) & Source=="InWeb_IM")
      ints <- unique(c(tempDf1$Gene1,tempDf1$Gene2,tempDf2$Gene1,tempDf2$Gene2))
    
    # get gold-standard interactions (InWeb3 or InWeb_IM ints with score = 1)
    } else if (type=="gs") {
      tempDf <- subset(inweb_table, (Gene1==bait | Gene2==bait) & Score==1)
      ints <- unique(c(tempDf$Gene1,tempDf$Gene2))
    }

    inwebDf <- data.frame(gene=inwebGenes[inwebGenes!=bait])
    inwebDf$significant <- inwebDf$gene %in% ints
  }

  return(inwebDf)
}
