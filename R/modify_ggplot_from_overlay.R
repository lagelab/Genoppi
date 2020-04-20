#' @title modify ggplot from overlay
#' @description a function used to modify a ggplot, e.g. a color
#' from an overlayed genelist. This is primarily used by shiny/plotly.
#' @param p a ggplot object also containing a data.frame \code{overlay}.
#' @note currently only handles color.
#' @export

modify_ggplot_from_overlay <- function(p){
  stopifnot(!is.null(p$overlay))
  p_overlay <- data.frame(
      gene = p$overlay$gene,
      color = ifelse(p$overlay$significant, p$overlay$col_significant, p$overlay$col_other)
  )
  # no duplicates allowed
  p_overlay = p_overlay[!duplicated(p_overlay),]
  # assign new color
  p$data[p$data$gene %in% p_overlay$gene, ]$color <- p_overlay$color
  # assign new size (todo)
  return(p)
}
