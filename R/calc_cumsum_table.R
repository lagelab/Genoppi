#' @title calculate the cumulative sum of frequencies
#' @param x a data.frame containign col.id and col.freq
#' @param col.id string. what item in \code{x} is the identifier?
#' @param col.freq string. what numeric column contains the frequency?
#' @keywords internal

calc_cumsum_table <- function(x, col.id, col.freq='Freq'){
  
  # check the input 
  stopifnot(col.id %in% colnames(x))
  stopifnot(col.freq %in% colnames(x))
  
  # get unique counts for each item
  counts = data.frame(item=x[[col.id]], Freq=x[[col.freq]])
  counts = counts[!duplicated(counts),]
  counts = counts[rev(order(counts[[col.freq]])),]
  counts$cumsum = cumsum(counts$Freq)
  
  
  return(counts)
  
}
