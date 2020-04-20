#' @title plot a basic scatte plot 
#' @description Draws a pair of specific replicates in a scatter plot.
#' @param df A data.frame containing at least gene, significant, replicate/triplicate columns.
#' @param repA string that is column in df. Expects name to be in the form of rep[0-9], e.g. 'rep1'.
#' @param repB string that is column in df.
#' @param size_point size of point.
#' @param col_significant color for significant protein interactors.
#' @param col_other color for other protein interactors.
#' @return a gg scatter plot.
#' @importFrom ggplot2 ggplot geom_point geom_abline labs theme_minimal
#' @export

plot_scatter_basic <- function(df, repA='rep1', repB='rep2', size_point = 3, col_significant = "#41AB5D", col_other = 'grey'){
  
  # check input
  if (!is.numeric(df[,repA])) stop('repA must be a numeric column.')
  if (!is.numeric(df[,repB])) stop('repB must be a numeric column.')
  
  # set default parameters
  df$color = ifelse(df$significant, col_significant, col_other)
  if (is.null(df$dataset)) df$dataset = 'pulldown'
  if (is.null(df$size)) df$size = 7
  
  # plot singlebasic scatter plot
  correlation = stats::cor(df[,repA], df[,repB])
  p = ggplot(df, mapping=aes_(x=as.name(repA), y=as.name(repB))) + 
    geom_point(alpha=1, size=size_point, color=ifelse(df$significant, "#41AB5D", "grey"), stroke = 0.6) +
    geom_abline(intercept=0, slope=1, linetype="longdash", size=0.2) +
    labs(title = paste("r =",format(correlation,digits=3))) + 
    xlab(bquote(.(gsub('(R|r)ep','Replicate ', repA))  ~log[2]~'(Fold Change)')) +
    ylab(bquote(.(gsub('(R|r)ep','Replicate ', repB))  ~log[2]~'(Fold Change)')) +
    theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank())
  
  # set parameters for downstream processing
  #p$visual = list(volcano=F, x=repA, y=repB)
  p$correlation = correlation
  return(p)
}


#' @title plot a list of basic scatter plots
#' @description Draw qll pairs of replicates in multiple scatter plot.
#' @param df A data.frame containing at least gene, significant, replicate/triplicate columns.
#' @param size_point size of point.
#' @param col_significant color for significant protein interactors.
#' @param col_other color for other protein interactors.
#' @return a list of gg scatter plots.
#' @export
plot_scatter_basic_all <- function(df, size_point = 3, col_significant = "#41AB5D", col_other = 'grey'){
  
  # check input
  expected_columns = c('logFC', 'FDR', 'pvalue', 'significant', 'gene')
  stop_invalid_columns(df, 'plot_scatte_basic_all', expected_columns)
  
  # enumerate all combinations replicate
  reps = regmatches(colnames(df), regexpr('rep[0-9]',colnames(df)))
  combinations = enumerate_replicate_combinations(length(reps))
  plts = lapply(1:nrow(combinations), function(i){
    repA = paste0('rep',combinations[i, 1])
    repB = paste0('rep',combinations[i, 2])
    name = paste0(repA,'.',repB)
    p = plot_scatter_basic(df, repA = repA, repB = repB, size_point = size_point, col_significant = col_significant, col_other = col_other)
    return(list(name = name, ggplot = p, correlation = p$correlation))
  })
  
  # set names
  names(plts) <- unlist(lapply(plts, function(x) x$name))
  return(plts)
}






