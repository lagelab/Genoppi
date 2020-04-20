#' @title set_names_by_dataset
#' @description the marker (e.g. colors) supplied to plotly must be in a global 
#' scheme with appropiate names (similar to a dict). This function generates
#' the appropiate mapping, so that the color scheme can be correctly plotted.
#' @param data a data.frame object.
#' @param overlay a data.frame object.
#' @param marker what should be organized?
#' @param by what should marker be organized by?
#' @family interactivity
#' @export
set_names_by_dataset <- function(data, overlay, marker = 'color', by = 'dataset'){
  
  # check
  stopifnot(all(c(marker, by) %in% colnames(data)))
  stopifnot(all(c(marker, by) %in% colnames(overlay)))
  
  # get pairs of colors
  tabl_data = data[,c(by,marker)]
  tabl_data = as.data.frame(tabl_data[!duplicated(tabl_data),])
  tabl_overlay = overlay[,c(by,marker)]
  tabl_overlay = as.data.frame(tabl_overlay[!duplicated(tabl_overlay),])
  tabl = rbind(tabl_data, tabl_overlay)
  
  # set colors
  global_colors <- stats::setNames(tabl[[marker]], tabl$dataset)
  
  return(global_colors)
  
}
