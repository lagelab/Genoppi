getExampleButton <- function(id, link_str){
  # HTML(paste(
  #   'Try a single',actionLink(NS(id, "example"), link_str), 'or'))
  actionLink(NS(id, "example"), link_str)
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

# update dataPath to example file when example button is pressed
getMultiExampleServer <- function(id, dataPathServerList, toggleSingleBasicServer) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$example, {
      dataPathServerList[[1]](example_file)
      dataPathServerList[[2]](example_file2)
      dataPathServerList[[3]](example_file3)
      ifelse(is.null(toggleSingleBasicServer()), 
             toggleSingleBasicServer(1),
             toggleSingleBasicServer(toggleSingleBasicServer()+1))
    })
  })
}



