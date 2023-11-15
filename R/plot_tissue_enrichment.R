#' @title plot tissue enrichment 
#' @description constructs a bar chart using ggplot to plot tissue enrichment.
#' @param data a data.frame
#' @param col.tissue string. Column containing tissue names.
#' @param col.value string. Column containing pvalue, FDR, enrichment for plotting.
#' @param xlab x label
#' @param ylab y label
#' @param pvalue.line pvalue threshold to be displayed via a horisontal line.
#' @family ggplot
#' @importFrom ggplot2 ggplot geom_bar xlab ylab aes_string geom_hline theme_minimal coord_flip
#' @importFrom plotly toRGB
#' @export

plot_tissue_enrichment <- function(data, col.tissue, col.value, xlab = 'tissue', ylab = 'Hypergeometric P-value', pvalue.line = NULL){
  
  p = ggplot(data, aes(x = !!rlang::sym(col.tissue), y = !!rlang::sym(col.value))) +
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
#' @param color.significant color of significant values. default is red.
#' @param color.other color of non-signfificant values. Default is orange.
#' @param col.value.order string that is either 'increasing' or 'decreasing'. Orders barplot accordingly.
#' @family plotly
#' @importFrom plotly plot_ly layout
#' @export

plotly_tissue_enrichment <- function(data, col.tissue, col.value, col.value.text = NULL, pvalue.line = NULL, 
                                     xlab = '', ylab = '', title = '', xlim = NULL, color.significant = 'red', 
                                     color.other = 'orange', col.value.order = NULL){
  
  # check input
  if (col.tissue %nin% colnames(data)) stop(paste(col.tissue),' was not found in column names!')
  if (col.value %nin% colnames(data)) stop(paste(col.value),' was not found in column names!')
  if (missing(col.value.text)) col.value.text <- col.value
  if ('significant' %nin% colnames(data)){
    data$significant = F
  } 
  
  # order by col value
  if (!is.null(col.value.order)){
    if (col.value.order == 'decreasing'){
      data[[col.tissue]] <- factor(data[[col.tissue]], levels = data[[col.tissue]][order(data[[col.value]])])
    } 
    if (col.value.order == 'increasing'){
      data[[col.tissue]] <- factor(data[[col.tissue]], levels = data[[col.tissue]][rev(order(data[[col.value]]))])
    }
  }

  # color scheme (constant for now)
  data$color <- ifelse(data$significant == T, color.significant, color.other)
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
          height = 900,
          showlegend = F)
  
  # axis ticks 
  xaxis.ticks <- list(
    title = xlab,
    autotick = FALSE,
    ticks = "outside",
    tick0 = 0,
    dtick = 0.25,
    tickcolor = toRGB("blue"),
    range = c(0, xlim),
    side = 'top'
  )
  
  yaxis <- list(
    title = ylab,
    tickangle = -35
  )
  
  
  p <- layout(p, title = title, yaxis = yaxis, xaxis = xaxis.ticks, shapes = list(vline(pvalue.line)))        
  
  return(p)
}




