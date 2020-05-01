#' @title layout for volcano plot.
#' @description Format axes.
#' @param p a plotly object.
#' @param height the heigh of the plotly object.
#' @param width the width of the plotly object.
#' @param legend_title the title of the legend.
#' @param orientation piped to plotly. Can be 'v'ertical or 'h'orisontal.
#' @param legend.x shift legend by x-axis.
#' @param legend.y shift legend by y-axis.
#' @family plotly
#' @importFrom plotly layout
#' @importFrom shiny HTML
#' @export

#add_layout_html_axes_volcano
add_plotly_layout_volcano <- function(p, height = NULL, width = NULL, legend_title = bold('Overlay'), 
                                         orientation = 'v', legend.x = NULL, legend.y = NULL){
  stopifnot(!is.null(p$data))
  p <- p %>% layout(xaxis = list(title = "log<sub>2</sub>(Fold change)", range=~c(min(p$data$logFC)-0.5, max(p$data$logFC)+0.5)),
                    yaxis = list(title = "-log<sub>10</sub>(<i>P</i>-value)", range=~c(min(-log10(p$data$pvalue)-0.5), max(-log10(p$data$pvalue))+0.5)),
                    legend=list(title=list(text=legend_title), orientation = orientation, x=legend.x, y=legend.y),
                    height = height, width = width, showlegend = TRUE)
  p
}




