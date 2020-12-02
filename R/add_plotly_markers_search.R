#' @title search volcano plots
#' @description Adds search markers to a plot. Requires a plotly object.
#' @param p a ggplot
#' @param genes a vector of genes
#' @note internal
#' @importFrom plotly add_markers
#' @export
#' @family shiny

add_plotly_markers_search <- function(p, genes, alpha = 0.5){
  
  # check input
  if (is.null(p$ggparams)) stop('p$ggparams is null. Expected a ggplot!')
  
  # adjusting transparency
  my_color = paste0('rgba(194, 194, 194, ', alpha,')')
  my_line_color = paste0('rgba(0, 0, 0, ', alpha,')')
  
  # check for genes
  p$search = p$data[grepl(genes, p$data$gene), ]
  if (nrow(p$search) > 0){
    p <- add_markers(p, data = p$search, x = p$ggparams$mapping$x, y = p$ggparams$mapping$y,
                     marker = list(color = my_color, size = 10, line = list(width=1.3, color = my_line_color)),
                     textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10),
                     hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
  p
}

