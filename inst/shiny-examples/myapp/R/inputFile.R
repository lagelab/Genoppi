inputFileWelcome <- function(id) {
  uiOutput(NS(id, 'input_file_welcome'))
}

inputFileSideBar <- function(id) {
  conditionalPanel("input.sidebarmenu === 'dashboard'",
                   uiOutput(NS(id, 'input_file_sidebar')),
                   uiOutput(NS(id, 'input_file_error')))
}

inputFileServer <- function(id, 
                            dataPathServer, 
                            toggleSingleBasicServer, 
                            errorValues) {
  if (!is.reactive(dataPathServer)){
    stop("dataPathServer passed to inputFileServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$input_file_welcome <- renderUI({
      fileInput(ns('input_file_welcome'), 
                'Upload a proteomic data file to get started!', 
                accept = files_accepted,
                placeholder = "No file selected")
    })
    
    output$input_file_sidebar <- renderUI({
      fileInput(ns('input_file_sidebar'), 
                'Upload user input file',
                accept = files_accepted,
                placeholder = "No file selected")
    })
    
    output$input_file_error <- renderUI({
      req(errorValues$columns_check)
      columns_err <- errorValues$columns_check
      if (columns_err != '') {stop(HTML(paste(columns_err, sep = "<br/>")))}
    })
    
    observeEvent(input$input_file_sidebar, {
      validate(need(!is.null(input$input_file_sidebar), 
                    'input_file_sidebar in inputFileServer is NULL'))
      dataPathServer(input$input_file_sidebar$datapath)
      ifelse(is.null(toggleSingleBasicServer()), 
             toggleSingleBasicServer(1),
             toggleSingleBasicServer(toggleSingleBasicServer()+1))
    })
    
    observeEvent(input$input_file_welcome, {
      validate(need(!is.null(input$input_file_welcome$datapath), 
                    'input_file_welcome in inputFileServer is NULL'))
      dataPathServer(input$input_file_welcome$datapath)
      ifelse(is.null(toggleSingleBasicServer()), 
             toggleSingleBasicServer(1),
             toggleSingleBasicServer(toggleSingleBasicServer()+1))
    })
  })
}
    