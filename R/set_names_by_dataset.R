#' @title set_names_by_dataset
#' @description the marker (e.g. colors) supplied to plotly must be in a global 
#' scheme with appropiate names (similar to a dict). This function generates
#' the appropiate mapping, so that the color scheme can be correctly plotted.
#' @param lst a data.frame or a list of data.frames that all contain the arguments
#' 'marker' and 'by' as column names.
#' @param marker what should be organized?
#' @param by what should marker be organized by?
#' @family interactivity
#' @export
#' 
set_names_by_dataset <- function(lst, marker = 'color', by = 'group'){
  
  # get the mapping of groups to colors, shapes etc.
  unique_markers_by_dataset = tabulate_markers(lst, marker, by)
  
  # combine all data.frames
  tabl = do.call(rbind, unique_markers_by_dataset)

  # check whether constraints are violated
  if (lun(tabl[[by]]) != length(tabl[[by]])) stop(paste0('Multiple non-unique column "',marker,'" that correspond to different "',by,
                                                             '". The mapping is ambigious and can not be constructed.'))
  # set colors
  global_colors <- stats::setNames(tabl[[marker]], tabl[[by]])
  
  
  return(global_colors)
  
}
