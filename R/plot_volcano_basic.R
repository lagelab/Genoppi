#' @title Plot basic volcano
#' @description takes in a data.frane with the columns gene, logFC, pvalue and significant 
#' to draw a volcano. Optionally, a column indicating 'color' (string) can be supplied to 
#' indicate the volcano color scheme and whether to draw names of specific proteins. See
#' details section for plotting aesthetics. 
#' @param df a data.frame with at least columns gene, logFC, pvalue and significant.
#' @param col_significant the color of significant proteins/rows.
#' @param col_other the color of non-significnt proteins/rows.
#' @param sig_text string. text for significant interactor to be displayed in legend.
#' @param insig_text string. Text for non-significant interactors to be displayed in legend.
#' @param gg.size the size of the points. 
#' @param shape the shape of the points. Default is 21 corresponding to circles.
#' @param stroke numeric. stroke width.
#' @param col_border string. a color for the outline of points.
#' @examples 
#' \dontrun{
#' 
#' # run example data
#' p = example_data %>% 
#' calc_mod_ttest() %>%
#'   id_enriched_proteins(fdr_cutoff = 0.1) %>%
#'   plot_volcano_basic() %>%
#'   plot_overlay(as.bait('BCL2')) %>%
#'   volcano_theme()
#'
#' # add a ggplot title
#' p + ggtitle('Example volcano plot') 
#' 
#' }
#' @export
#' @importFrom ggplot2 ggplot geom_point geom_hline geom_vline xlab ylab theme aes_ aes aes_string geom_text


plot_volcano_basic <- function(df, col_significant = "#41AB5D", col_other = 'grey', sig_text = '(enriched)', 
                               insig_text = '(not enriched)', gg.size = 3, shape = 21, stroke = 0.2, 
                               col_border = NULL){
  
  # check input
  stop_invalid_columns(df,'plot_volcano_basic',c('gene','logFC', 'pvalue', 'significant'))
  
  # default parameters
  df$color <- ifelse(df$significant, col_significant, col_other)
  if (is.null(df$dataset)) df$dataset = 'proteomic data'
  if (is.null(df$size)) df$size = 7
  if (is.null(df$shape)) df$shape = shape
  if (is.null(df$col_border)) df$col_border = unlist(ifelse(is.null(col_border), list(df$color), col_border))
  
  # discriminate between significant and non-significant 
  df = append_to_column(df, sig_text = sig_text, insig_text = insig_text, to = 'group')
  global_colors = set_names_by_dataset(list(df), by = 'group')
  global_shapes = set_names_by_dataset(list(df), by = 'group', marker = 'shape')
  global_borders = set_names_by_dataset(list(df), by = 'group', marker = 'col_border')
  
  # setup plotting  
  p <- ggplot(df, aes(x = logFC, y = -log10(pvalue), fill = group, shape = group, color = group)) +
    geom_point(alpha=1, size=gg.size, stroke = stroke) + 
    geom_hline(yintercept=0, color="black") + 
    geom_vline(xintercept=0, color="black") +
    xlab(bquote(log[2]*"[fold change]")) + 
    ylab(bquote(-log[10]*"["*italic(.("P"))*"-value]")) + 
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


