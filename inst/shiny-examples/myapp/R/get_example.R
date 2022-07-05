getExampleButton <- function(id){
  actionLink(NS(id, "example"), "EXAMPLE DATA")
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
