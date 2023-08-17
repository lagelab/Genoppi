inputFileWelcome <- function(id) {
  uiOutput(NS(id, 'input_file_welcome'))
}

inputFileSideBar <- function(id, sidebarmenu_condition) {
  conditionalPanel(paste0("input.sidebarmenu === '", sidebarmenu_condition,"'"),
                   uiOutput(NS(id, 'input_file_sidebar')),
                   uiOutput(NS(id, 'input_file_error')))
}

inputFileServer <- function(id, 
                            dataPathServer, 
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
      validate(need(!is.null(input$input_file_sidebar$datapath), 
                    'input_file_sidebar in inputFileServer is NULL'))
      dataPathServer(input$input_file_sidebar$datapath)
    })
    
    observeEvent(input$input_file_welcome, {
      validate(need(!is.null(input$input_file_welcome$datapath), 
                    'input_file_welcome in inputFileServer is NULL'))
      dataPathServer(input$input_file_welcome$datapath)
    })
  })
}
    
multiFileName <- function(id, sidebarmenu_condition) {
  conditionalPanel(paste0("input.sidebarmenu === '", sidebarmenu_condition,"'"),
                   uiOutput(NS(id, 'input_file_name')))
}

multiFileNameServer <- function(id, default_name, filename) {
  if (!is.reactive(filename)){
    stop("filename passed to basicPlotParamServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$input_file_name <- renderUI({
      textInput(ns('input_file_name'), 
                paste(default_name, "name"), 
                value = default_name, 
                width = '80%', 
                placeholder = default_name)
      })
    
    observeEvent(input$input_file_name, {
      validate(need(input$input_file_name != '', 
                    'input_file_name in multiFileNameServer is an empty string'))
      filename(input$input_file_name)
    })
  })
}