#' @title add unity line
#' @description adds a unity line to plotly object
#' @param p a plotly object
#' @param data the data object used to draw the plotly object. By
#' default, the function will look for the data object within the
#' plotly object.
#' @param color color of line
#' @param width width of line
#' @param dash dashed or solid?
#' @param opacity opacity of line.
#' @export

add_line_unity <- function(p, data=NULL, color = 'black', width = 0.5, dash = 'solid', opacity = 0.5){
  
  if (is.null(data)) data = p$data
  stopifnot(!is.null(data))
  limmin = floor(min(c(data[[quo_name(p$ggparams$mapping$x)]], data[[quo_name(p$ggparams$mapping$x)]])))
  limmax = ceiling(max(c(data[[quo_name(p$ggparams$mapping$x)]],data[[quo_name(p$ggparams$mapping$x)]])))
  p1 = add_trace(p, x = limmin:limmax, y = limmin:limmax, 
                 type = 'scatter', 
                 mode = 'lines',
                 opacity = opacity,
                 line = list(color= color, width=width, dash=dash), showlegend=F)
  
  return(p1)
}
