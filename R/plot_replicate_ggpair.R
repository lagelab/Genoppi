#' plot ggpair plot for given columns from the given data frame
#'
#' @param data data frame containing data to be plotted
#' @param columns vector containing column names of the data frame provided
#' @param overlay_sigificance Boolean indicating whether the sigificant proteins
#'  should be overlaid with green color dots (can only be used when a Boolean 
#'  significant column is present).
#' @param significant_color color for the proteins identified as significantly enriched
#' @param insignificant_color color for the proteins not identified as significantly enriched
#' @param title optional title for the ggpair plot
#'
#' @return a ggpair plot showing correlation between samples, controls, 
#' sample and contorl, or replicate logFC.
#'
#' @examples
#' \dontrun{
#' p = example_data %>% 
#'   calc_mod_ttest() %>%
#'   id_significant_proteins(fdr_cutoff = 0.1) %>%
#'   plot_replicate_ggpair()
#' }
#' @importFrom GGally ggpairs wrap
#' @export
plot_replicate_ggpair <- function(data, 
                                  columns, 
                                  overlay_sigificance=FALSE, 
                                  significant_color="#41AB5D",
                                  insignificant_color="#808080",
                                  title=""){
  stopifnot(length(columns) > 0)
  columns <- sort(columns)
  colors = "black"
  if (overlay_sigificance) {
    stopifnot(!is.null(data$significant))
    colors <- data$significant
    colors[colors] <- significant_color
    colors[!as.logical(colors)] <- insignificant_color
  }
  cor_font_size <- 6
  plot_label_font_size <- 16
  ticks_font_size <- 10
  p <- ggpairs(data[columns],
          title=title,
          lower=list(
            continuous=wrap("points",size=0.1, color=colors)),
          upper=list(
            continuous=wrap("cor", size=cor_font_size))) +
    theme_minimal(base_size = 20)+
    theme(panel.grid.minor = element_blank())
  ncol <- p$ncol
  nrow <- p$nrow
  plots_index <- expand.grid(y=1:nrow, x=1:ncol)
  pList <- apply(plots_index, MARGIN = 1, function(yx) {
    i=yx[2]
    j=yx[1]
    pTmp <- GGally::getPlot(p, i=yx[2], j=yx[1])
    if (i<j) { # plot index count from top left to bottom right (row by row)
      pTmp <- pTmp+theme(panel.grid.major = element_blank())
    }
    pTmp
  })
  
  axis_lab <- ""
  if (all(grepl("sample|control", columns))) {
    axis_lab <- "log2 intensity value"
  } else if (all(grepl("rep", columns))) {
    axis_lab <- "logFC"
  } else {
    print("Cannot guess axis label based on columns input to plot_replicate_ggpair.")
  }
  
  gp <- GGally::ggmatrix(pList, 
                   nrow=nrow, ncol=ncol,
                   xAxisLabels = p$xAxisLabels,
                   yAxisLabels = p$yAxisLabels, 
                   xlab=axis_lab,
                   ylab=axis_lab) + 
    theme(text = element_text(size=plot_label_font_size),
          axis.text = element_text(size=ticks_font_size), 
          panel.border = element_blank())
  gp
}