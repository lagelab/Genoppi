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
set_names_by_dataset <- function(lst, marker = 'color', by = 'dataset'){
  
  # accept both lists and data.frames
  if (inherits(lst, "data.frame")) lst = list(lst)
  stopifnot(is.character(marker))
  stopifnot(is.character(by))
  
  # get unique markers by each data.set
  unique_markers_by_dataset = lapply(lst, function(df) {
    if (!inherits(df, "data.frame")) stop('expected a data.frame as a list item!')
    if (marker %nin% colnames(df)) stop(paste('expected column', marker,'in data.frame!'))
    if (by %nin% colnames(df)) stop(paste('expected column', by,'in data.frame!'))
    tabl = df[,c(by,marker)]
    tabl = as.data.frame(tabl[!duplicated(tabl),])
    return(tabl)
  })
  
  # combine all data.frames
  tabl = do.call(rbind, unique_markers_by_dataset)

  
  # check uniqueness
  if (lun(tabl$dataset) != length(tabl$dataset)) stop(paste0('Multiple non-unique column "',marker,'" that correspond to different "',by,
                                                             '". The mapping is ambigious and can not be constructed.'))
  # set colors
  global_colors <- stats::setNames(tabl[[marker]], tabl$dataset)
  
  
  return(global_colors)
  
}
