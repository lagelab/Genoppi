#' @title validate reference data.frame
#' @description Input a data.frame and check whether it contains a pre-specified
#' set of valid columns. Invalid columns will be omitted alongside a warning.
#' @param df a data.frame
#' @param warn should a warning be issued about the columns that have been discarded?
#' @param valid a vector of valid downstream ggplot/plotly.
#' @return a data.frame
#' @family overlay
#' @export

validate_reference <- function(df, warn = T, valid = c('gene',
                                             'accession_number',
                                             'col_significant',
                                             'col_other',
                                             'col_border',
                                             'shape',
                                             'dataset',
                                             'group',
                                             'stroke',
                                             'alt_label',
                                             'label', 
                                             'label_size', 
                                             'size', 
                                             'symbol',
                                             'pLI', 
                                             'opacity', 
                                             'size', 
                                             'size_gg',
                                             'legend_order')){
  
  bool = colnames(df) %in% valid
  cols = colnames(df)[!bool]
  if (any(!bool) & warn) warning(paste0('columns: >', paste(cols, collapse=', '),'< from reference data.frame are not ggplot compatible and were ignored.'))
  return(df[bool])
}
