library(genoppi) # Genoppi >= 0.4.10 required
library(shiny)

# get data files to be documented
data.files = tools::file_path_sans_ext(list.files('data/', full.names = F))
data.documentation = file.path('man//',paste0(data.files, '.Rd'))

# function for combining list of strings
documentation_to_html <- function(content){
  
  content_wo_title = content[names(content) %nin% 'title']
  title = bold(content[['title']])
  header_content = unlist(lapply(names(content_wo_title), function(header){
    return(paste0('<br>', italics(header),': ',content[[header]]))
  }))
  
  title_header_content = paste(c(title, header_content), collapse = ' ')
  return(title_header_content)
  
}

# go over each file and parse
for (file in data.documentation){
  
  # write documentation files
  content = null_omit(genoppi:::parse_rd(readLines(file), combine.with = ' '))
  content_html = documentation_to_html(content)
  newfile = paste0(tools::file_path_sans_ext(basename(file)),'.info')
  write(content_html, file = paste0('inst/shiny-examples/myapp/documentation/', newfile))
  
}


















