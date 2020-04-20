#' @title plot overlay
#' @description Takes a ggplot2 object and overlays points from a reference. 
#' The function uses the mapping and plot environment from a previous ggplot to add an overlay
#' with a matching 'reference' data.frame. This yields a new plot with items that intersect the 
#' the original ggplot data and the reference data. Can be applied iteratively. 
#' The user can input certain columns in the reference to gain more control over the plotting 
#' parameters of individual points. See notes for additional details.
#' 
#' @note The following data.frame headers for a reference are accepted:
#' \itemize{
#'  \item{"gene"}{ A string tht indicates the gene name.}
#'  \item{"label"} {A boolen that indicates whether the label should be plotted.}
#'  \item{"label_size"}{A numeric that indicates the size of the label items.}
#' }
#' See ?validate_reference or additional details.
#' 
#' @param p A ggplot object. Usually passed down from \code{plot_volcano_basic} or \code{plot_scatter_basic}.
#' @param reference a list of data.frames that are preferably named. The name of the list will passed down to
#' the data.frame as the column 'dataset'. Alternatively, the dataset can have a column name dataset.
#' @param match by what string should the ggplot and overlay be merged? Default is 'gene'.
#' @param label A boolean. This will overwrite the \code{label} column in the reference data.frame.
#' @param label.size numeric. Size of label. This will overwrite the \code{label.size} column in the reference data.frame.
#' @param label.color the color of the label. Default is black.
#' @param label.box.padding Amount of padding around bounding box. See \code{?ggrepel::geom_text_repel} 
#' for more details.
#' @param label.point.padding Amount of padding around label. See \code{?ggrepel::geom_text_repel}.
#' @param label.arrowhead.size The size of the arrowhead. 0 means no arrowhead.
#' 
#' @return a ggplot
#' 
#' @importFrom ggplot2 geom_point quo_name ggsave
#' @import ggrepel
#' @export

plot_overlay <- function(p, reference, match = 'gene', label = NULL, label.size = NULL, label.color = 'black', 
                         label.box.padding = 0.30, label.point.padding = 0.50, label.arrowhead.size = 0.01) {
  
  # check for allowed input
  if (!inherits(reference, "list")) stop('argumnt reference must be a named list.')
  
  # convert reference to a single data.frame and omit non informative columns
  overlay = do.call(rbind, lapply(names(reference), function(x) to_overlay_data(reference[[x]], x)))
  plot.data = p$plot_env$df[,colnames(p$plot_env$df) %nin% c('dataset','color', 'size')]
  overlay =  merge(plot.data, validate_reference(overlay), by = match)
  
  # add the overlay to the ggplot
  p1 = p + ggplot2::geom_point(data = overlay, 
                 mapping = aes_string(x=p$mapping$x, y=p$mapping$y),
                 fill = ifelse(overlay$significant, as.character(overlay$col_significant), as.character(overlay$col_other)),
                 size = overlay$gg.size,
                 shape = overlay$shape,
                 stroke = 0.75)
  
  # annotate plot
  p1 = p1 + ggrepel::geom_text_repel(collapse_labels(overlay[unlist(ifelse(is.null(label), list(overlay$label), list(label))),]), 
                       mapping=aes(label = gene),
                       color=label.color,
                       size=ifelse(is.null(label.size), overlay$label_size, label.size),
                       arrow=arrow(length=unit(label.arrowhead.size, 'npc')),
                       box.padding=unit(label.box.padding, "lines"),
                       point.padding=unit(label.point.padding, "lines"))
                  
  
  # make running list of overlay
  if (!is.null(p1$overlay)) {p1$overlay = rbind(p1$overlay, overlay)} else {p1$overlay = overlay}
  return(p1)
  
}
