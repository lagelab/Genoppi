#' @title tabulate markers
#' @description a function that tabulates a data.frame or list of data.frames
#' by a certain marker. This means, that it will find all unique mappings from 'marker'
#' to 'by' in the form of a table.
#' @param lst a data.frame or a list of data.frames that all contain the arguments
#' 'marker' and 'by' as column names.
#' @param marker what should be organized?
#' @param by what should marker be organized by?
#' @family interactivity
#' @return a table
#' @export
#' 

tabulate_markers <- function(lst, marker, by){
  
  # check input
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
  
  return(unique_markers_by_dataset)
  
}