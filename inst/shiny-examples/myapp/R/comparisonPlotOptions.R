ComparisonPlotInputBox <- function(id) {
  box(
    title = "Comparison plot options", width = NULL, solidHeader = TRUE,
    status = "primary", collapsible = TRUE, collapsed = FALSE,
    fluidRow(
      column(6, uiOutput(NS(id, "color_theme_both_sig"))),
      column(6, uiOutput(NS(id, "color_theme_first_sig"))),
      column(6, uiOutput(NS(id, "color_theme_second_sig"))),
      column(6, uiOutput(NS(id, "color_theme_insig")))
    )
  )
}


ComparisonPlotParamServer <- function(id,
                                      bothSigColorServer,
                                      firstSigColorServer,
                                      secondSigColorServer,
                                      insigColorServer) {
  if (!is.reactive(bothSigColorServer)) {
    stop("bothSigColorServer passed to ComparisonPlotParamServer is not reactive")
  }
  if (!is.reactive(firstSigColorServer)) {
    stop("firstSigColorServer passed to ComparisonPlotParamServer is not reactive")
  }
  if (!is.reactive(secondSigColorServer)) {
    stop("secondSigColorServer passed to ComparisonPlotParamServer is not reactive")
  }
  if (!is.reactive(insigColorServer)) {
    stop("insigColorServer passed to ComparisonPlotParamServer is not reactive")
  }
  req(bothSigColorServer, firstSigColorServer, secondSigColorServer, insigColorServer)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$color_theme_both_sig <- renderUI({
      label <- (HTML("Colors for significant proteins"))
      colourpicker::colourInput(ns("color_both_sig_in"), label,
        value = bothSigColorServer(), showColour = "both",
        palette = c("limited"),
        allowedCols = allowed_colors
      )
    })
    output$color_theme_first_sig <- renderUI({
      label <- (HTML("Colors for significant proteins in first file only"))
      colourpicker::colourInput(ns("color_first_sig_in"), label,
        value = firstSigColorServer(), showColour = "both",
        palette = c("limited"),
        allowedCols = allowed_colors
      )
    })
    output$color_theme_second_sig <- renderUI({
      label <- (HTML("Colors for significant proteins in second file only"))
      colourpicker::colourInput(ns("color_second_sig_in"), label,
        value = secondSigColorServer(), showColour = "both",
        palette = c("limited"),
        allowedCols = allowed_colors
      )
    })
    output$color_theme_insig <- renderUI({
      label <- (HTML("Colors for insignificant proteins"))
      colourpicker::colourInput(ns("color_insig_in"), label,
        value = insigColorServer(), showColour = "both",
        palette = c("limited"),
        allowedCols = allowed_colors
      )
    })

    observeEvent(input$color_both_sig_in, {
      sigColorServer(input$color_both_sig_in)
    })
    observeEvent(input$color_first_sig_in, {
      sigColorServer(input$color_first_sig_in)
    })
    observeEvent(input$color_second_sig_in, {
      sigColorServer(input$color_second_sig_in)
    })
    observeEvent(input$color_insig_in, {
      insigColorServer(input$color_insig_in)
    })
  })
}