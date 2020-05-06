#' @title Append a string to a column
#' @description Appends a dynamic string to a column condition on whether
#' the another column 'significant' is true or false.
#' @param data a data or overlay data.frame with \code{dataset} as column.
#' @param sig_text what should be the significance text?
#' @param insig_text what should be the insignificance text?
#' @param from what column should be appended?
#' @param to where should be appended column be inserted?
#' @param nchar_max integer. maximum amount of characters (incl spaces) in the group. NULL 
#' means that there is no limit. 
#' @note In order to correctly draw plotly points
#' the identifier (dataset) must have a unique colors associated with it.
#' This function appends the dataset column with a significance text. 
#' @export
#' 

append_to_column <- function(data, sig_text = '(enriched)', insig_text = '(not enriched)', from = 'dataset', to = 'dataset', nchar_max = NULL){
  stopifnot(sig_text != insig_text)
  stopifnot(from %in% colnames(data))
  stopifnot('significant' %in% colnames(data))
  data$sigtext = ifelse(data$significant, as.character(sig_text), as.character(insig_text))
  data[[to]] = apply(data[,c(from,'sigtext')], 1, paste, collapse = ' ')
  
  # cutoff part of the column if it exceeds the max 
  # allowed number of characters and nchar_nax != NULL
  if (!is.null(nchar_max)){
    bool = nchar(data[[to]]) > nchar_max
    
    if (sum(bool) > 0){
    
      newname = strsplit.nchar(data[[to]][bool], nchar_max)
      data[[to]][bool] <- unlist(newname)
    
    }
    
  }
  
  data$sigtext = NULL
  return(data)
}

