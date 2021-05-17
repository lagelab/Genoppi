#' @title get shiny errors
#' @description parses a data.frame and returns a string potential problems that might occour downstream.
#' @param df a data.frame
#' @export

get_shiny_errors <- function(df){
  
  # check column names
  check_columns_names = check_input(df)
  cnames = colnames(df)
  msg = ''
  warning = ''
  
  # The input format was not accepted
  if (all(!unlist(check_columns_names$check))){
    msg[[1]] = paste('The input columns were not recognized. Are you sure you are using the correct input? The header names are case sensitive. See the supplementary protocol on what column names are accepted.')
    return(msg)
  }
  
  # check columns
  if ('gene' %in% cnames){
    if (!is_cols(df, 'gene', function(x) is.character(x) | is.factor(x))){msg = paste(msg, 'exepected column gene to be of characters!')}
  }
  if ('accession_number' %in% cnames){
    if (!is_cols(df, 'accession_number', function(x) is.character(x) | is.factor(x))){msg = paste(msg, 'exepcted column accession_number to be of characters!')}
  }
  if (any(grepl('rep[0-9]', cnames))){
    if (!is_cols(df, 'rep[0-9]', is.numeric)){msg = paste(msg, 'exepcted column(s) rep[0-9] to be numeric!')}
  }
  if ('logFC' %in% cnames){
    if (!is_cols(df, 'logFC', is.numeric)){msg = paste(msg, 'exepcted column logFC to be numeric')}
  }
  if ('pvalue' %in% cnames){
    if (!is_cols(df, 'pvalue', is.numeric)){msg = paste(msg, 'exepcted column pvalue to be numeric')}
  }
  if ('FDR' %in% cnames){
    if (!is_cols(df, 'FDR', is.numeric)){msg = paste(msg, 'exepcted column FDR to be numeric')}
  }
  
  return(msg)
}




