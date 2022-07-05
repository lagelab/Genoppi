toggleSingleBasicServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactiveVal(value=NULL)
  })
}