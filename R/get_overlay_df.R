#' @title get overlay data.frame
#' @description Uses a ggplot and a reference data.frame to generate
#' 'match' genes that are present in both the plot and the reference data.
#'  Subsequently, plot information for the individual points are extracted
#'  and can now be used as an overlay. See {?plot_overlay}
#' 
#' @param p A ggplot object. Usually passed down from \code{plot_volcano_basic} or \code{plot_scatter_basic}.
#' @param reference a list of data.frames that are preferably named. The name of the list will passed down to
#' the data.frame as the column 'dataset'. Alternatively, the dataset can have a column name dataset.
#' @param match by what string should the ggplot and overlay be merged? Default is 'gene'.
#' @param sig_text string. text for enriched interactors to be displayed in legend. 
#' @param insig_text string. Text for non-enriched interactors to be displayed in legend.
#' @param legend_nchar_max maximum amount of allowed characters in the legend.
#' @param nchar_max_collapse what charcter should be used for line break? Default is HTML line break \code{"<br>".}
#' @param dont_inherit the parameters/columns that should not be inherited from the plotting environment.
#' 
#' @export

get_overlay_df <- function(p, reference, match = 'gene', sig_text = NULL, insig_text = NULL, 
                           legend_nchar_max = NULL, nchar_max_collapse = NULL,
                           dont_inherit = c('dataset','color', 'size', 'size_gg', 'shape', 'group', 'col_border')){
  
  # check for allowed input
  if (!inherits(reference, "list")) stop('argumnt reference must be a named list.')
  
  # only allow overlay significant references
  if (!is.null(reference$significant)){
    df <- df[df$significant, ]
  }
  
  ## collapse references to a single data.frame and omit non informative columns
  
  # Add essential data need for downstream plotting (in plotly as well)
  overlay = do.call(rbind, lapply(names(reference), function(x) to_overlay_data(reference[[x]], x, rm.sig = T)))
  
  # remove items from plot that we do not want to inherit
  plot.data = p$plot_env$df[,colnames(p$plot_env$df) %nin% dont_inherit]
  
  # merge plot data and reference
  overlay =  merge(plot.data, validate_reference(overlay, warn = F), by = match)
  
  # change color scheme
  overlay$color = ifelse(overlay$significant, as.character(overlay$col_significant), as.character(overlay$col_other))
  
  # append data with overlay specific text
  overlay = append_to_column(overlay, 
                             sig_text = ifelse(is.null(sig_text), p$settings$sig_text, sig_text),
                             insig_text = ifelse(is.null(insig_text), p$settings$insig_text, insig_text),
                             to = 'group', 
                             nchar_max = legend_nchar_max, 
                             nchar_max_collapse = nchar_max_collapse)
  
  return(overlay)
  
}

