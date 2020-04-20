#' @title add genoppi trace
#' @description adding a trace with new data to plotly is non-trivial. This function takes
#' care of adding data correctly with the correct coloring.
#' @param p a plot_ly() object. 
#' @param data a data.frame with columns dataset, gene, 
#' @param parameters a list of parameters or an environement generated with \code{environment()}.
#' @param stroke_width numeric. the line width/stroke of a given point.
#' @param legend Boolean. should legend be displayed?
#' @note global variable 'global_colors' must be specified using setNames().
#' @importFrom plotly add_trace
#' @export

add_genoppi_trace <- function(p, data, parameters, stroke_width = 0.2, legend = F){
  
  # pass previous environment to function
  global_colors = parameters$global_colors
  global_symbols = parameters$global_symbols
  coords = parameters$ggparams

  # set legend order in trace
  if (!any(is.na(data$legend_order))) data$dataset = factor(data$dataset, levels = unique(data$dataset[data$legend_order]))
  
  #if (any(data$size > 7.5)) browser()
    
  #if (is.null(data$accession_number)) browser()
  
  # make trace
  p1 <- add_trace(p, data = data, 
                  type = 'scatter',
                  mode = 'markers',
                  x = coords$mapping$x, 
                  y = coords$mapping$y, 
                  color = ~dataset, 
                  colors = global_colors,
                  symbol = ~dataset, 
                  symbols = global_symbols,
                  size = ~size, 
                  key = ~gene,
                  name = ~dataset,
                  text = ~gene,
                  opacity = 0.9,
                  marker = list(cmin = 0,
                                cmax = 1, 
                                line = list(width=stroke_width, color = "black"), 
                                sizemode = 'diameter',
                                opacity = data$opacity),
                  hoverinfo = "text", 
                  hovertemplate = ~paste(paste0(bold(ifelse(!is.na(gene), as.character(gene), '<NA>')),
                                                bold(ifelse(!is.na(accession_number), paste0(' [',accession_number,']'), '')),
                                                ", FDR=", signif(FDR, digits = 3),'<br>',
                                                ifelse(!is.na(data$alt_label), alt_label, dataset), 
                                                sep = "<br>")),
                  textposition = ~ifelse(logFC>0,"top right","top left"),
                  legendgroup = NULL,
                  showlegend = legend)
  
  return(p1)
}

