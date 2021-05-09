#' @title Plot basic volcano
#' @description Plot protein-level data in a basic volcano plot. Can be chained with \code{plot_overlay} to produce overlays.
#' 
#' @param df data.frame with at least columns \code{gene}, \code{logFC}, \code{pvalue} and \code{significant}.
#' @param col_significant string. The color of significant proteins/genes.
#' @param col_other string. The color of non-significant proteins/genes.
#' @param sig_text string. Text for significant interactor to be displayed in legend.
#' @param insig_text string. Text for non-significant interactors to be displayed in legend. 
#' @param size_gg numeric. The size of the points. 
#' @param shape integer. The shape of the points. Default to 21 (circles).
#' @param stroke numeric. Stroke width.
#' @param col_border string. A color for the outline of points.
#' @param plot_segments boolean. Plot volcano plot axis segments, i.e. two lines with x and y axis intercept.
#' 
#' @examples 
#' \dontrun{
#' p = example_data %>% 
#' calc_mod_ttest() %>%
#'   id_significant_proteins(fdr_cutoff = 0.1) %>%
#'   plot_volcano_basic() %>%
#'   plot_overlay(as.bait('BCL2')) 
#' p + ggtitle('Example volcano plot') + theme_classic()
#' 
#' }
#' @export
#' @importFrom ggplot2 ggplot geom_point geom_hline geom_vline xlab ylab theme aes_ aes aes_string geom_text


plot_volcano_basic <- function(df, col_significant = "#41AB5D", col_other = 'grey', sig_text = '(significant)', 
                               insig_text = '(not significant)', size_gg = 3, shape = 21, stroke = 0.2, 
                               col_border = NULL, plot_segments = F){
  
  # check input
  stop_invalid_columns(df,'plot_volcano_basic',c('gene','logFC', 'pvalue', 'significant'))
  if (sig_text == insig_text) insig_text = paste0(insig_text, ' ')
  
  # default parameters
  df$color <- ifelse(df$significant, col_significant, col_other)
  if (is.null(df$dataset)) df$dataset = 'proteomic data'
  if (is.null(df$size)) df$size = 7
  if (is.null(df$size_gg)) df$size_gg = size_gg
  if (is.null(df$shape)) df$shape = shape
  if (is.null(df$col_border)) df$col_border = unlist(ifelse(is.null(col_border), list(df$color), col_border))
  
  # discriminate between significant and non-significant 
  df = append_to_column(df, sig_text = sig_text, insig_text = insig_text, to = 'group')
  global_colors = set_names_by_dataset(list(df), by = 'group')
  global_shapes = set_names_by_dataset(list(df), by = 'group', marker = 'shape')
  global_borders = set_names_by_dataset(list(df), by = 'group', marker = 'col_border')
  
  # setup plotting  
  p <- ggplot(df, aes(x = logFC, y = -log10(pvalue), fill = group, shape = group, color = group)) +
    geom_point(alpha=1, size=size_gg, stroke = stroke) 
    
  if (plot_segments){
    p <- p  +
      geom_hline(yintercept=0, color="black") + 
      geom_vline(xintercept=0, color="black")
  }

  # setup aesthetics
  p <- p + xlab(bquote(log[2]*"(Fold change)")) + 
    ylab(bquote(-log[10]*"("*italic(.("P"))*"-value)")) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    scale_fill_manual(values = global_colors) +
    scale_shape_manual(values = global_shapes) +
    scale_color_manual(values = global_borders)
  
  # settings used downstream
  p$settings <- list(sig_text = sig_text, insig_text = insig_text)
  
  return(p)
}

