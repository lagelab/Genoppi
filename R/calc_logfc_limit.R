#' @title calculate logfc limits
#' @description returns the maximum absoluate values plus 0.5 of the logFC.
#' @param df a data.frame with column logFC
#' @param logfc_direction the direction of logfc. A string of either
#' 'negative', 'positive' or 'both'.
#' @export
#' @family shiny

calc_logfc_limit <- function(df, logfc_direction = 'both'){
  
  stopifnot('logFC' %in% colnames(df))
  
  if (logfc_direction == 'negative'){
    limit <- abs(min(df$logFC))
    limit <- round(limit+0.5, 1)
  } else if (logfc_direction == 'positive'){
    limit <- max(df$logFC)
    limit <- round(limit+0.5, 1)
  } else {
    limit <- max(max(df$logFC), abs(min(df$logFC)))
    limit <- round(limit+0.5, 1)
  }
  return(limit)
}