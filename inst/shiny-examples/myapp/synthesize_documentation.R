#library(genoppi) # Genoppi >= 0.4.10 required
library(shiny)

# run this script from start to end, in order to generate 
# documentation that is used in the shiny app.

# clean up previous documentation
old.files <- list.files('inst/shiny-examples/myapp/documentation/', pattern='\\.info$', full.names=T)
file.remove(old.files) 

# get data files to be documented
data.files <- tools::file_path_sans_ext(list.files('data/', pattern='\\.rda$', full.names=F))
data.documentation <- file.path('man//',paste0(data.files, '.Rd'))

# convert double spaces to new line
doublespacenewline <- function(x) gsub('\\ \\ +', '<br>', x)

# function for combining list of strings
documentation_to_html <- function(content){
  
  content_wo_title <- content[names(content) %nin% 'title']
  title <- genoppi:::bold(content[['title']])
  
  # add newlines to references
  content_wo_title$references <- doublespacenewline(content_wo_title$references)

  # Create header content
  header_content <- unlist(lapply(names(content_wo_title), function(header){
    return(paste0('<br>', genoppi:::italics(header),': ', content_wo_title[[header]]))
  }))
  
  title_header_content <- paste(c(title, header_content), collapse=' ')
  return(title_header_content)
  
}

# go through roxygen titles, to get alphabetic order
headers <- unlist(lapply(data.documentation, function(file){
	content=genoppi:::null_omit(genoppi:::parse_rd(readLines(file), what=c("title"), combine.with=' '))$title}))
data.documentation.ordered <- data.documentation[order(headers)]

# go over each file and parse
count <- 0
for (file in data.documentation.ordered){
  
  count <- count + 1
  # write documentation files
  content <- genoppi:::null_omit(genoppi:::parse_rd(readLines(file), what=c("title", "description", "source", "references"), combine.with=' '))
  content_html <- documentation_to_html(content)
  content_html <- gsub('\\\\url\\{','',content_html)
  newfile <- paste0(count,'_',tools::file_path_sans_ext(basename(file)),'.info')
  write(content_html, file=paste0('inst/shiny-examples/myapp/documentation/', newfile))
  
}

