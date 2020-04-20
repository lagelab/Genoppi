#' @title Append a string to a column
#' @description Appends a dynamic string to a column condition on whether
#' the another column 'significant' is true or false.
#' @param data a data or overlay data.frame with \code{dataset} as column.
#' @param sig_text what should be the significance text?
#' @param insig_text what should be the insignificance text?
#' @note In order to correctly draw plotly points
#' the identifier (dataset) must have a unique colors associated with it.
#' This function appends the dataset column with a significance text. 
#' @export
#' 

append_to_column <- function(data, sig_text = '(enriched)', insig_text = '(not enriched)'){
  stopifnot(sig_text != insig_text)
  stopifnot('dataset' %in% colnames(data))
  stopifnot('significant' %in% colnames(data))
  data$sigtext = ifelse(data$significant, as.character(sig_text), as.character(insig_text))
  data$dataset = apply(data[,c('dataset','sigtext')], 1, paste, collapse = ' ')
  data$sigtext = NULL
  return(data)
}

