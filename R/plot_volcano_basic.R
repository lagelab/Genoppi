#' @title Plot basic volcano
#' @description takes in a data.frane with the columns gene, logFC, pvalue and significant 
#' to draw a volcano. Optionally, a column indicating 'color' (string) can be supplied to 
#' indicate the volcano color scheme and whether to draw names of specific proteins.
#' @param df a data.frame with at least columns gene, logFC, pvalue and significant.
#' @param col_significant the color of significant proteins/rows.
#' @param col_other the color of non-significnt proteins/rows.
#' @param gg.size the size of the points. 
#' @export
#' @importFrom ggplot2 ggplot geom_point geom_hline geom_vline xlab ylab theme aes_ aes aes_string geom_text
#' 




plot_volcano_basic <- function(df, col_significant = "#41AB5D", col_other = 'grey', gg.size = 3){
  
  # check input
  stop_invalid_columns(df,'plot_volcano_basic',c('gene','logFC', 'pvalue', 'significant'))
  
  # set default parameters
  df$color <- ifelse(df$significant, col_significant, col_other)
  if (is.null(df$dataset)) df$dataset = 'pulldown'
  if (is.null(df$size)) df$size = 7
  
  # setup plotting  
  p <- ggplot(df, aes(x = logFC, y = -log10(pvalue))) +
    geom_point(alpha=1, size=gg.size, color=df$color, stroke = 0.6) +   
    geom_hline(yintercept=0, color="black") + 
    geom_vline(xintercept=0, color="black") +
    xlab(bquote(log[2]*"[fold change]")) + 
    ylab(bquote(-log[10]*"["*italic(.("P"))*"-value]")) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())

  return(p)
}
