#' @title parse documentation files for content
#' @description A function that parsed markdown documentation files
#' that has been written in \code{devtools::document}. 
#' @param content a vector of strings.
#' @param what a vector of search terms.
#' @param raw boolean. Should the raw data be returned?
#' @param combine boolean. Should the vectors in the list be combined?
#' @param combine.with string. What should the vectors be collapsed with.
#' @examples 
#' \dontrun{
#' content = readLines('man/gtex_table.Rd')
#' parse_rd(content)
#' }


parse_rd <- function(content, what = c('description', 'title', 'format', 'references'), raw = F, 
                     combine = T, combine.with = '\n'){
  
  n = length(content)
  
  parsed_content = lapply(what, function(x) {
    
    #tag = paste0('\\@', x)
    
    if (any(grepl(x, content))){
      
      string_start = paste0('\\\\', x,'\\{')
      string_end = '}'
      
      bool_start = grepl(string_start, content)
      bool_end = grepl(string_end, content)
      
      pos_start = which(bool_start)
      pos_end = which(bool_end)
      
      from = pos_start
      to = min(pos_end[pos_end >= from])
      
      parsed = content[from:to]
      
      if (!raw){
        parsed = gsub(paste0(string_start,'|',string_end), '', parsed)
        parsed = parsed[nchar(parsed) != 0]
        
        if (combine){
          parsed = paste(parsed, collapse = combine.with)
        }
        
      }
      
      return(parsed)
      
    }
  
  })
  
  names(parsed_content) <- what
  return(parsed_content)
  
  
}
