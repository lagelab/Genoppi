#' @title superimpose genesets onto plots
#' @description 
#' Takes a ggplot2 object and superimoses genes from a reference data.frame. This data.frame
#' can be constructed manually or generated with a variety of functions, e.g. \code{get_inweb_list},
#' \code{get_irefindex_list} or \code{get_tissue_list}.
#' 
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
#' See ?validate_reference for additional details.
#' 
#' @param p A ggplot object passed down from \code{plot_volcano_basic} or \code{plot_scatter_basic}.
#' @param reference a named list of data.frames. 
#' @param match string. By what string should the ggplot and overlay be merged? Default is 'gene'.
#' @param size_gg numeric. Size of ggplot points.
#' @param col_significant string. Color of superimposed genes that are significant.
#' @param col_other string. Color of superimposed genes that are non-significant.
#' @param label boolean. This will overwrite the \code{label} column in the reference data.frame.
#' @param label_size numeric. Size of label. This will overwrite the \code{label.size} column in the reference data.frame.
#' @param label_color string. The color of the label. Default is black.
#' @param label_box_padding numeric. Amount of padding around bounding box. See \code{?ggrepel::geom_text_repel}.
#' @param label_max_overlaps integer. Exclude text labels that overlap too many things. See \code{?ggrepel::geom_text_repel}.
#' @param label_point_padding numeric. Amount of padding around label. See \code{?ggrepel::geom_text_repel}.
#' @param label_arrowhead_size numeric. The size of the arrowhead. 0 means no arrowhead.
#' @param legend_nchar_max integer. Maximum amount of allowed characters in the legend.
#' @param nchar_max_collapse string. What character should be used for line break? Default is HTML line break \code{"<br>".}
#' @param stroke numeric. The width of the outline/borders. 
#' @param sig_text string. Text for significant interactors to be displayed in legend. 
#' @param insig_text string. Text for non-significant interactors to be displayed in legend.
#' 
#' @examples 
#' \dontrun{
#' df <- example_data %>%
#'   calc_mod_ttest() %>%
#'   id_significant_proteins()
#' 
#' # overlay simple  a bait
#' df %>% 
#'   plot_volcano_basic() %>%
#'   plot_overlay(as.bait('BCL2')) 
#'
#' # make a custom overlay with custom colors
#' myoverlay = data.frame(gene = c('FUS', 'RBMX'),
#'                        col_significant = c('cyan', 'blue'),
#'                        col_other = c('grey', 'grey'), 
#'                        dataset = c('group 1', 'group 2'))
#' 
#' # plot overlay
#' df %>% 
#'   plot_volcano_basic() %>%
#'   plot_overlay(list(overlay = myoverlay)) 
#'
#' # plot multiple overlays
#' df %>% 
#'   plot_volcano_basic() %>%
#'   plot_overlay(list(overlay = myoverlay)) %>%
#'   plot_overlay(as.bait('BCL2')) 
#' }
#' @importFrom ggplot2 geom_point quo_name ggsave scale_fill_manual scale_shape_manual scale_color_manual guides guide_legend
#' @import ggrepel
#' @importFrom magrittr %>%
#' @export


plot_overlay <- function(p, reference, match = 'gene', size_gg = 3.5, stroke = 0.75, sig_text = NULL, insig_text = NULL,
                         label = NULL, label_size = NULL, label_color = 'black', col_significant = NULL, col_other = NULL,
                         label_box_padding = 0.30, label_point_padding = 0.50, label_arrowhead_size = 0.01, label_max_overlaps = 10,
                         legend_nchar_max = NULL, nchar_max_collapse = '<br>') {
  

  # let the user add colors to overlays if needed
  if (!is.null(col_significant)) reference[[1]]$col_significant <- col_significant
  if (!is.null(col_other)) reference[[1]]$col_other <- col_other
  
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
                       max.overlaps = label_max_overlaps,
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
