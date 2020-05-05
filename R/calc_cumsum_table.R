#' @title calculate the cumulative sum of frequencies
#' @param x a data.frame containign col.id and col.freq
#' @param col.id string. what item in \code{x} is the identifier?
#' @param col.freq string. what numeric column contains the frequency?
#' @return 
#' 
#' \item{item}{the identifier item}
#' \item{Freq}{the frequency of the item}
#' \item{n}{the reversed cumulative sum of frequencies of items. i.e. the 
#' revsersed cumulative sum of the tabulated frequencies.}
#' 
#' @export
#' 

calc_cumsum_table <- function(x, col.id, col.freq='Freq'){
  
  # check the input 
  stopifnot(col.id %in% colnames(x))
  stopifnot(col.freq %in% colnames(x))
  
  # get unique counts for each item
  counts = data.frame(item=x[[col.id]], Freq=x[[col.freq]])
  counts = counts[!duplicated(counts),]
  counts = counts[rev(order(counts[[col.freq]])),]
  
  # get ranked order
  revcumsum = cumsum(rev(table(counts$Freq)))
  revcumsum = data.frame(Freq=names(revcumsum),n=revcumsum)
  combined = merge(counts, revcumsum, by = 'Freq')
  combined = combined[(order(combined$n)), c('item','Freq','n')]
  
  return(combined)
  
}
