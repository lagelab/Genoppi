#' @title plot tissue enrichment 
#' @description constructs a bar chart using ggplot to plot tissue enrichment.
#' @param data a data.frame
#' @param col.tissue string. Column containing tissue names.
#' @param col.value string. Column containing pvalue, FDR, enrichment for plotting.
#' @param xlab x label
#' @param ylab y label
#' @param pvalue.line pvalue threshold to be displayed via a horisontal line.
#' @family ggplot
#' @export

plot_tissue_enrichment <- function(data, col.tissue, col.value, xlab = 'tissue', ylab = '-log10(Hypergeometric P-value)', pvalue.line = 0.05){
  
  p = ggplot(data, aes_string(x = col.tissue, y = col.value)) +
    geom_bar(stat="identity", color = 'black', position = 'dodge', fill = 'orange') +
    geom_hline(yintercept=pvalue.line, linetype = "dashed", color = 'red') +
    xlab(xlab) + ylab(ylab) + theme_minimal() + coord_flip() 
  
  return(p)
  
}

#' @title plot tissue enrichment 
#' @description constructs a bar chart using ggplot to plot tissue enrichment.
#' @param data a data.frame
#' @param col.tissue string. Column containing tissue names.
#' @param col.value string. Column containing pvalue, FDR, enrichment for plotting.
#' @param col.value.text string. Column containing text that should be displayed when hovering over bar.
#' @param pvalue.line p-value horizontal line.
#' @param xlab string. x label.
#' @param ylab string. y label.
#' @param title string. title of plot.
#' @param xlim vector of length 2.
#' @family plotly
#' @export
#' @export

plotly_tissue_enrichment <- function(data, col.tissue, col.value, col.value.text = NULL, pvalue.line = NULL, 
                                     xlab = '', ylab = '', title = '', xlim = NULL){
  
  # color scheme (constant for now)
  data$color <- ifelse(data$significant == 'significant', 'red', 'orange')
  params.colors = set_names_by_dataset(data, marker = 'color', by = 'significant') 
  
  # main plotting
  p = plot_ly(data, 
          type = 'bar',
          x = data[[col.value]], 
          y = data[[col.tissue]], 
          name = data[[col.tissue]],
          text = NULL,
          #text = data[[col.tissue]],
          color = data[['significant']],
          colors = params.colors,
          textposition = 'auto', 
          marker = list(line = list(color = 'black', width = 1)),
          hoverinfo = "text", 
          hovertemplate = ~data[[col.value.text]],
          showlegend = F)
  
  # axis ticks 
  xaxis.ticks <- list(
    title = xlab,
    autotick = FALSE,
    ticks = "outside",
    tick0 = 0,
    dtick = 0.25,
    tickcolor = toRGB("blue"),
    range = c(0, xlim)
  )
  
  yaxis <- list(
    title = ylab,
    tickangle = -35
  )
  
  
  p <- layout(p, title = title, yaxis = yaxis, xaxis = xaxis.ticks, height = 900, shapes = list(vline(pvalue.line)))        
  
  return(p)
}




