#' @title Retrieve InWeb data for a given bait
#' @description Use inweb_hash data to get InWeb interactors and non-interactors of bait.
#' @param bait name of bait protein
#' @return data.frame containing gene and significant columns for all InWeb genes (significant=T for InWeb interactors of bait). NULL if bait not found in InWeb.
#' @importFrom hash hash keys values
#' @export

get_inweb_list <- function(bait){
  inwebDf <- NULL
  
  if (bait %in% hash::keys(inweb_hash)){
    inwebDf <- data.frame(gene=keys(inweb_hash))
    inwebDf$significant <- inwebDf$gene %in% inweb_hash[[bait]]
  }

  return(inwebDf)
}
