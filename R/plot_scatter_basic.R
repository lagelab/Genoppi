#' @title plot a basic scatter plot of replicates
#' 
#' @description Plot protein-level data in a sactter plot.
#' 
#' @param df data.frame with at least columns \code{gene}, \code{significant} and some \code{rep[0-9]}.
#' @param repA string that is column in df. Expects name to be in the form of rep[0-9], e.g. 'rep1'.
#' @param repB string that is column in df. Expects name to be in the form of rep[0-9], e.g. 'rep1'.
#' @param size_gg numeric. Size of point.
#' @param col_significant string. Color for significant protein interactors.
#' @param col_other string. Color for other protein interactors.
#' @param sig_text string. Text for significant interactor to be displayed in legend.
#' @param insig_text string. Text for non-significant interactors to be displayed in legend.
#' @param shape integer. The shape of the point. Default is 21 (circle).
#' @param stroke numeric. The stroke width.
#' @param col_border string. The color of the borders/outline.
#' 
#' @examples 
#' \dontrun{
#' # run example data
#' p = example_data %>% 
#' calc_mod_ttest() %>%
#'   id_significant_proteins(fdr_cutoff = 0.1) %>%
#'   plot_scatter_basic() %>%
#'   plot_overlay(as.bait('BCL2'))
#'   
#' p + ggtitle('Example scatter plot')
#' }
#' @importFrom ggplot2 ggplot geom_point geom_abline labs theme_minimal
#' @export


plot_scatter_basic <- function(df, repA='rep1', repB='rep2', size_gg = 3, col_significant = "#41AB5D", col_other = 'grey', 
                               sig_text = '(significant)', insig_text = '(not significant)', shape = 21, stroke = 0.2, col_border = NULL){
  
  # check input
  if (!is.numeric(df[,repA])) stop('repA must be a numeric column.')
  if (!is.numeric(df[,repB])) stop('repB must be a numeric column.')
  if (sig_text == insig_text) insig_text = paste0(insig_text, ' ')
  
  # set default parameters
  df$color = ifelse(df$significant, col_significant, col_other)
  if (is.null(df$dataset)) df$dataset = 'proteomic data'
  if (is.null(df$size)) df$size = 7
  if (is.null(df$shape)) df$shape = 21
  if (is.null(df$col_border)) df$col_border = df$color
  if (is.null(df$col_border)) df$col_border = unlist(ifelse(is.null(col_border), list(df$color), col_border))
  
  # discriminate between significant and non-significant 
  df = append_to_column(df, sig_text = sig_text, insig_text = insig_text, to = 'group')
  global_colors = set_names_by_dataset(list(df), by = 'group')
  global_shapes = set_names_by_dataset(list(df), by = 'group', marker = 'shape')
  global_borders = set_names_by_dataset(list(df), by = 'group', marker = 'col_border')
  
  # plot singlebasic scatter plot
  correlation = stats::cor(df[,repA], df[,repB])
  p = ggplot(df, mapping=aes_(x=as.name(repA), y=as.name(repB), fill = as.name("group"), shape = as.name('group'), color = as.name('group'))) + 
    geom_point(alpha=1, size=size_gg, stroke = stroke) +
    geom_abline(intercept=0, slope=1, linetype="longdash", size=0.2) +
    labs(title = paste("r =",format(correlation,digits=3))) + 
    xlab(bquote(.(gsub('(R|r)ep','Replicate ', repA))  ~log[2]~'[Fold Change]')) +
    ylab(bquote(.(gsub('(R|r)ep','Replicate ', repB))  ~log[2]~'[Fold Change]')) +
    theme_minimal() + theme(panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank()) +
    scale_fill_manual(values = global_colors) +
    scale_shape_manual(values = global_shapes) +
    scale_color_manual(values = global_borders)
    theme_classic()
  
  # set parameters for downstream processing
  #p$visual = list(volcano=F, x=repA, y=repB)
  p$settings <- list(sig_text = sig_text, insig_text = insig_text)
  p$correlation = correlation
  
  return(p)
}


#' @title plot a list of basic scatter plots
#' @description Draw qll pairs of replicates in multiple scatter plot.
#' @param df A data.frame containing at least gene, significant, replicate/triplicate columns.
#' @param size_gg size of point.
#' @param col_significant color for significant protein interactors.
#' @param col_other color for other protein interactors.
#' @return a list of gg scatter plots.
#' @export

plot_scatter_basic_all <- function(df, size_gg = 3, col_significant = "#41AB5D", col_other = 'grey'){
  
  # check input
  expected_columns = c('logFC', 'FDR', 'pvalue', 'significant', 'gene')
  stop_invalid_columns(df, 'plot_scatter_basic_all', expected_columns)
  
  # enumerate all combinations replicate
  reps = regmatches(colnames(df), regexpr('rep[0-9]',colnames(df)))
  combinations = enumerate_replicate_combinations(length(reps))
  plts = lapply(1:nrow(combinations), function(i){
    repA = paste0('rep',combinations[i, 1])
    repB = paste0('rep',combinations[i, 2])
    name = paste0(repA,'.',repB)
    p = plot_scatter_basic(df, repA = repA, repB = repB, size_gg = size_gg, col_significant = col_significant, col_other = col_other)
    return(list(name = name, ggplot = p, correlation = p$correlation))
  })
  
  # set names
  names(plts) <- unlist(lapply(plts, function(x) x$name))
  return(plts)
}

