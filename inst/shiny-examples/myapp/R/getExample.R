getExampleButton <- function(id){
  HTML(paste(
    'Try a single',actionLink(NS(id, "example"), "example file"), 'or'))
  
}

# update dataPath to example file when example button is pressed
getExampleServer <- function(id, dataPathServer, toggleSingleBasicServer) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$example, {
      dataPathServer(example_file)
      ifelse(is.null(toggleSingleBasicServer()), 
             toggleSingleBasicServer(1),
             toggleSingleBasicServer(toggleSingleBasicServer()+1))
    })
  })
}
