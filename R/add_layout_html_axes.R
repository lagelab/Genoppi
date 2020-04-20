#' @title layout for scatterplot
#' @description format axes of a plotly object.
#' @param p a plotly object.
#' @param title the title of the plot.
#' @param legend_title the title of the legend.
#' @param height the heigh of the plotly object.
#' @param width the width of the plotly object.
#' @importFrom plotly layout
#' @importFrom shiny HTML
#' @family shiny
#' @export
add_layout_html_axes_scatterplot <- function(p, title='', legend_title = bold('Overlay'), width=NULL, height=NULL){
  
  toreplicate <- function(x) gsub('(R|r)ep','Replicate ', x)
  stopifnot(!is.null(p$data))
  p <- p %>% layout(xaxis = list(title = paste(toreplicate(quo_name(p$ggparams$mapping$x)),"log<sub>2</sub>(Fold change)"), 
                                 range=~c((min(p$data[[quo_name(p$ggparams$mapping$x)]], p$data[[quo_name(p$ggparams$mapping$y)]]))-1, 
                                          (max(p$data[[quo_name(p$ggparams$mapping$x)]], p$data[[quo_name(p$ggparams$mapping$y)]]))+1)), 
                    yaxis = list(title = paste(toreplicate(quo_name(p$ggparams$mapping$y)),"log<sub>2</sub>(Fold change)"), 
                                 range=~c((min(p$data[[quo_name(p$ggparams$mapping$x)]], p$data[[quo_name(p$ggparams$mapping$y)]]))-1, 
                                          (max(p$data[[quo_name(p$ggparams$mapping$x)]], p$data[[quo_name(p$ggparams$mapping$y)]]))+1)), 
                    title = title, #titlefont = list(size=15), 
                    legend=list(title=list(text=legend_title)),
                    height = height, width =width)
  p
}

#' @title layout for volcano plot.
#' @description Format axes.
#' @param p a plotly object.
#' @param height the heigh of the plotly object.
#' @param width the width of the plotly object.
#' @param legend_title the title of the legend.
#' @family shiny
#' @importFrom plotly layout
#' @importFrom shiny HTML
#' @export
add_layout_html_axes_volcano <- function(p, height = NULL, width = NULL, legend_title = bold('Overlay')){
  stopifnot(!is.null(p$data))
  p <- p %>% layout(xaxis = list(title = "log<sub>2</sub>(Fold change)", range=~c(min(p$data$logFC)-0.5, max(p$data$logFC)+0.5)),
                    yaxis = list(title = "-log<sub>10</sub>(<i>P</i>-value)", range=~c(min(-log10(p$data$pvalue)-0.5), max(-log10(p$data$pvalue))+0.5)),
                    legend=list(title=list(text=HTML(legend_title))),
                    height = height, width = width, showlegend = TRUE)
  p
}




