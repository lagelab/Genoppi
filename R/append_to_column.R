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
#' @param nchar_max_collapse what should be the line seperator when nchar_max is specified?
#' @note In order to correctly draw plotly points
#' the identifier (dataset) must have a unique colors associated with it.
#' This function appends the dataset column with a significance text. 
#' @export

append_to_column <- function(data, sig_text = '(enriched)', insig_text = '(not enriched)', from = 'dataset', to = 'dataset', 
                             nchar_max = NULL, nchar_max_collapse = '<br>'){
  
  # check input
  stopifnot(sig_text != insig_text)
  stopifnot(from %in% colnames(data))
  stopifnot('significant' %in% colnames(data))
  
  # append with significant/nonsignificant text
  data$sigtext = ifelse(data$significant, as.character(sig_text), as.character(insig_text))
  data[[to]] = apply(data[,c(from,'sigtext')], 1, paste, collapse = ' ')
  
  # cutoff part of the column if it exceeds the max 
  # allowed number of characters and nchar_nax != NULL
  if (!is.null(nchar_max)){
    bool = nchar(data[[to]]) > nchar_max
    
    if (sum(bool) > 0){
      
      tmpname = data[[to]][bool]
      tmpname = gsub('\\_','\\%v\\ ', tmpname)
      newname = unlist(lapply(tmpname, function(x) gsub('(\\%v\\ )|(\\%v)', '\\_' ,paste(strwrap(x, width = nchar_max), collapse = nchar_max_collapse))))
      
      # assign to data
      data[[to]][bool] <- unlist(newname) 
      
    }
    
  }

  data$sigtext = NULL
  return(data)
}

