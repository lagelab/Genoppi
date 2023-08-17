baitServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactiveVal(value = NULL))
  })
}
goiServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactiveVal(value = NULL))
  })
}
goiAlphaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactiveVal(value = NULL))
  })
}

basicPlotInputBox <- function(id) {
  box(
    title = "Basic plot options", width = NULL, solidHeader = TRUE, 
    status = "primary", collapsible = TRUE, collapsed = FALSE,
    # fluidRow(
    #   column(12, uiOutput(NS(id,'select_scatterplot_ui')))
    # ),
    fluidRow(
      column(6, uiOutput(NS(id, "bait_label"))),
      column(6, uiOutput(NS(id, "goi_search"))),
      
    ),
    fluidRow(conditionalPanel(
      condition = "input.goi_search != ''", ns=NS(id),
      column(12, uiOutput(NS(id, "goi_alpha")))),
    ),
    fluidRow(
      column(6, uiOutput(NS(id, "color_theme_indv_sig"))),
      column(6, uiOutput(NS(id,"color_theme_indv_insig")))
    )
  )
}


basicPlotParamServer <- function(id, 
                                 baitServer,
                                 goiServer,
                                 goiAlphaServer,
                                 sigColorServer, 
                                 insigColorServer) {
  if (!is.reactive(baitServer)){
    stop("baitServer passed to basicPlotParamServer is not reactive")}
  if (!is.reactive(goiAlphaServer)){
    stop("goiAlphaServer passed to basicPlotParamServer is not reactive")}
  if (!is.reactive(goiServer)){
    stop("goiServer passed to basicPlotParamServer is not reactive")}
  if (!is.reactive(sigColorServer)){
    stop("sigColorServer passed to basicPlotParamServer is not reactive")}
  if (!is.reactive(insigColorServer)){
    stop("insigColorServer passed to basicPlotParamServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$bait_label <- renderUI({
      textInput(ns("bait_label"), "Input bait (HGNC symbol, e.g. BCL2)")
    })
    output$goi_search <- renderUI({
      textInput(ns("goi_search"), "Search HGNC symbol")
    })
    output$goi_alpha <- renderUI({
      sliderInput(ns("goi_alpha"), 'Adjust overlay alpha',
                  min = 0, max = 1, value = 0.8, step = 0.05)
    })
    output$color_theme_indv_sig <- renderUI({
      label = (HTML('Colors for significant proteins'))
      colourpicker::colourInput(ns('color_indv_sig_in'), label, 
                                value = sigColorServer(), showColour = 'both', 
                                palette = c( "limited"),
                                allowedCols = allowed_colors)
    })
    output$color_theme_indv_insig <- renderUI({
      label = (HTML('Colors for insignificant proteins'))
      colourpicker::colourInput(ns('color_indv_insig_in'), label, 
                                value = insigColorServer(), showColour = 'both', 
                                palette = c( "limited"), 
                                allowedCols = allowed_colors)
    })
    
    observeEvent(input$bait_label, {
      if (input$bait_label == "") {
        baitServer(NULL)
      } else {
        baitServer(input$bait_label)
      }
    })
    observeEvent(input$goi_search, {
      if (input$goi_search == "") {
        goiServer(NULL)
      } else {
        goiServer(input$goi_search)
      }
      shinyjs::toggle(id=ns("goi_alpha"), condition = input$goi_search == "")
    })
    observeEvent(input$goi_alpha, {
      goiAlphaServer(input$goi_alpha)
    })
    observeEvent(input$color_indv_sig_in, {
      sigColorServer(input$color_indv_sig_in)
    })
    observeEvent(input$color_indv_insig_in, {
      insigColorServer(input$color_indv_insig_in)
    })
  })
}