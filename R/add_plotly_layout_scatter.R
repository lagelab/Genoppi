#' @title layout for scatterplot
#' @description format axes of a plotly object.
#' @param p a plotly object.
#' @param title the title of the plot.
#' @param legend_title the title of the legend.
#' @param height the height of the plotly object.
#' @param width the width of the plotly object.
#' @param orientation piped to plotly. Can be 'v'ertical or 'h'orisontal.
#' @param legend.x shift legend by x-axis.
#' @param legend.y shift legend by y-axis.
#' @importFrom plotly layout
#' @importFrom shiny HTML
#' @importFrom magrittr %>%
#' @family plotly
#' @export

# add_layout_html_axes_scatterplot
add_plotly_layout_scatter <- function(p, title='', legend_title = bold('Overlay'), width=NULL, height=NULL, 
                                             orientation = 'v', legend.x = 0, legend.y = 0){
  
  toreplicate <- function(x) gsub('(R|r)ep','Replicate ', x)
  stopifnot(!is.null(p$data))
  p <- p %>% layout(xaxis = list(title = paste(toreplicate(quo_name(p$ggparams$mapping$x)),"log<sub>2</sub>(Fold change)"), 
                                 range=~c((min(p$data[[quo_name(p$ggparams$mapping$x)]], p$data[[quo_name(p$ggparams$mapping$y)]]))-1, 
                                          (max(p$data[[quo_name(p$ggparams$mapping$x)]], p$data[[quo_name(p$ggparams$mapping$y)]]))+1)), 
                    yaxis = list(title = paste(toreplicate(quo_name(p$ggparams$mapping$y)),"log<sub>2</sub>(Fold change)"), 
                                 range=~c((min(p$data[[quo_name(p$ggparams$mapping$x)]], p$data[[quo_name(p$ggparams$mapping$y)]]))-1, 
                                          (max(p$data[[quo_name(p$ggparams$mapping$x)]], p$data[[quo_name(p$ggparams$mapping$y)]]))+1)), 
                    title = title, #titlefont = list(size=15), 
                    legend=list(title=list(text=legend_title), orientation = orientation,
                                x=legend.x, y=legend.y),
                    height = height, width =width)
  p
}