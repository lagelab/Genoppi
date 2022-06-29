getExampleButton <- function(id){
  actionLink(NS(id, "example"), "EXAMPLE DATA")
}

# update dataPath to example file when example button is pressed
getExampleServer <- function(id, dataPathServer) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$example, {
      dataPathServer(example_file)
      TRUE
    })
  })
}