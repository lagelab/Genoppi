#' plot ggpair plot for given columns from the given data frame
#'
#' @param data data frame containing data to be plotted
#' @param columns vector containing column names of the data frame provided
#' @param overlay_sigificance Boolean indicating whether the sigificant proteins should be overlaid with green color dots (can only be used when a Boolean significant column is present).
#' @param title optional title for the ggpair plot
#'
#' @return a ggpair plot showing correlation between samples, controls, sample and contorl, or replicate logFC.
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
plot_replicate_ggpair <- function(data, columns, overlay_sigificance=FALSE, title=""){
  stopifnot(length(columns) > 0)
  colors = "black"
  if (overlay_sigificance) {
    stopifnot(!is.null(data$significant))
    colors <- data$significant
    colors[colors] <- "#41AB5D"
    colors[!as.logical(colors)] <- "#808080"
  }
  ggpairs(data[columns],
          title=title,
          lower=list(continuous=wrap("points",size=0.05, color=colors))) +
    theme_minimal()
}