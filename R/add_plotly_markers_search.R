#' @title search volcano plots
#' @description Adds search markers to a plot. Requires a plotly object.
#' @param p a ggplot
#' @param genes a vector of genes
#' @note internal
#' @importFrom plotly add_markers
#' @export
#' @family shiny

add_plotly_markers_search <- function(p, genes){
  
  if (is.null(p$ggparams)) stop('p$ggparams is null. Expected a ggplot!')
  p$search = p$data[grepl(genes, p$data$gene), ]
  if (nrow(p$search) > 0){
    p <- add_markers(p, data = p$search, x = p$ggparams$mapping$x, y = p$ggparams$mapping$y,
                     marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
                     textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10),
                     hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
  p
}

