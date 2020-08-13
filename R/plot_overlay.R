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
#'  \item{"label.size"}{A numeric that indicates the size of the label items.}
#' }
#' See ?validate_reference or additional details.
#' 
#' @param p A ggplot object. Usually passed down from \code{plot_volcano_basic} or \code{plot_scatter_basic}.
#' @param reference a list of data.frames that are preferably named. The name of the list will passed down to
#' the data.frame as the column 'dataset'. Alternatively, the dataset can have a column name dataset.
#' @param match by what string should the ggplot and overlay be merged? Default is 'gene'.
#' @param size_gg size of ggplot points.
#' @param label A boolean. This will overwrite the \code{label} column in the reference data.frame.
#' @param label_size numeric. Size of label. This will overwrite the \code{label.size} column in the reference data.frame.
#' @param label_color the color of the label. Default is black.
#' @param label_box_padding Amount of padding around bounding box. See \code{?ggrepel::geom_text_repel} 
#' for more details.
#' @param label_point_padding Amount of padding around label. See \code{?ggrepel::geom_text_repel}.
#' @param label_arrowhead_size The size of the arrowhead. 0 means no arrowhead.
#' @param legend_nchar_max maximum amount of allowed characters in the legend.
#' @param nchar_max_collapse what charcter should be used for line break? Default is HTML line break \code{"<br>".}
#' @param stroke numeric. The width of the outline/borders. 
#' @param sig_text string. text for enriched interactors to be displayed in legend. 
#' @param insig_text string. Text for non-enriched interactors to be displayed in legend.
#' 
#' @return a ggplot
#' 
#' @importFrom ggplot2 geom_point quo_name ggsave
#' @import ggrepel
#' @examples 
#' \dontrun{
#' df <- example_data %>%
#'   calc_mod_ttest() %>%
#'   id_enriched_proteins()
#' 
#' # overlay simple  a bait
#' df %>% 
#'   plot_volcano_basic() %>%
#'   plot_overlay(as.bait('BCL2')) %>%
#'   volcano_theme()
#'
#' # make a custom overlay
#' myoverlay = data.frame(gene = c('FUS', 'RBMX'),
#'                        col_significant = c('cyan', 'blue'),
#'                        col_other = c('grey', 'grey'), 
#'                        dataset = c('group 1', 'group 2'))
#' 
#' # plot overlay
#' df %>% 
#'   plot_volcano_basic() %>%
#'   plot_overlay(list(overlay = myoverlay)) %>%
#'   volcano_theme() 
#'
#' # plot multiple overlays
#' df %>% 
#'   plot_volcano_basic() %>%
#'   plot_overlay(list(overlay = myoverlay)) %>%
#'   plot_overlay(as.bait('BCL2')) %>%
#'   volcano_theme() 
#' }
#' @export


plot_overlay <- function(p, reference, match = 'gene', size_gg = 3.5, stroke = 0.75, sig_text = NULL, insig_text = NULL,
                         label = NULL, label_size = NULL, label_color = 'black', 
                         label_box_padding = 0.30, label_point_padding = 0.50, label_arrowhead_size = 0.01,
                         legend_nchar_max = NULL, nchar_max_collapse = '<br>') {
  

  # collapse references to a single data.frame and omit non informative columns, this
  # function does all the hard work, by ensuring that information from the plot is being inherited.
  overlay = get_overlay_df(p, reference, match, sig_text, insig_text, legend_nchar_max, nchar_max_collapse)
  
  # reset color scales using the original plot data, the previous overlay and the current overlay. This
  # is needed since we need to dynamincally add colors, shapes, borders.
  global_colors = set_names_by_dataset(null_omit(list(p$data, overlay, p$overlay)), by = 'group')
  global_shapes = set_names_by_dataset(null_omit(list(p$data, overlay, p$overlay)), by = 'group', marker = 'shape')
  global_borders = set_names_by_dataset(null_omit(list(p$data, overlay, p$overlay)), by = 'group', marker = 'col_border')
  p$scales$scales[[1]] <- scale_fill_manual(values = global_colors)
  p$scales$scales[[2]] <- scale_shape_manual(values = global_shapes)
  p$scales$scales[[3]] <- scale_color_manual(values = global_borders)
  
  # add the overlay to the ggplot
  p1 = p + ggplot2::geom_point(data = overlay, 
                 mapping = aes_string(x=p$mapping$x, 
                                      y=p$mapping$y, 
                                      fill = p$mapping$fill, 
                                      shape = p$mapping$shape),
                 size = ifelse(is.null(size_gg), overlay$size_gg, size_gg), # gg.size
                 stroke = stroke,
                 alpha = overlay$opacity) 
  
  # add guides for maintaining legend size
  p1 = p1 + guides(fill=guide_legend(override.aes=list(size = 3))) 
  
  # annotate plot
  p1 = p1 + ggrepel::geom_text_repel(collapse_labels(overlay[unlist(ifelse(is.null(label), list(overlay$label), list(label))),]), 
                       mapping=aes(label = gene),
                       color=label_color,
                       size=ifelse(is.null(label_size), overlay$label_size, label_size),
                       arrow=arrow(length=unit(label_arrowhead_size, 'npc')),
                       box.padding=unit(label_box_padding, "lines"),
                       point.padding=unit(label_point_padding, "lines"))
  
  # change data.frame if arguments are passed directly to function (for downstream plotly rendering)
  if (!is.null(label)) overlay$label <- label
  
  # make running list of overlay
  if (!is.null(p1$overlay)) {p1$overlay = rbind(p1$overlay, overlay)} else {p1$overlay = overlay}
  return(p1)
  
}
