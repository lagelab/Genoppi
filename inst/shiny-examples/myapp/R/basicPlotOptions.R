basicPlotInputBox <- function(id) {
  box(
    title = "Basic plot options", width = NULL, solidHeader = TRUE, 
    status = "primary", collapsible = TRUE, collapsed = FALSE,
    # fluidRow(
    #   column(12, uiOutput(NS(id,'select_scatterplot_ui')))
    # ),
    # TODO new button for selecting type of mod t-test
    # fluidRow(
    #   column(12, uiOutput('a_select_mod_ttest_ui')) 
    # ),
    fluidRow(
      column(6, uiOutput(NS(id, "color_theme_indv_sig"))),
      column(6, uiOutput(NS(id,"color_theme_indv_insig")))
    )
  )
}


basicPlotParamServer <- function(id, sigColorServer, insigColorServer) {
  if (!is.reactive(sigColorServer)){stop("sigColorServer passed to basicPlotParamServer is not reactive")}
  if (!is.reactive(insigColorServer)){stop("insigColorServer passed to basicPlotParamServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    output$color_theme_indv_sig <- renderUI({
      ns <- session$ns
      # validate(need(a_file_pulldown_r()  != '', ""))
      # label = (HTML(paste(c('Colors for ',monitor_significance_thresholds()$sig, 'and', monitor_logfc_threshold()$sig))))
      label = (HTML('Colors for significant interactors'))
      colourpicker::colourInput(ns('color_indv_sig_in'), label, 
                                value = sigColorServer(), showColour = 'both', 
                                palette = c( "limited"),
                                allowedCols = allowed_colors)
    })
    observeEvent(input$color_indv_sig_in, {
      sigColorServer(input$color_indv_sig_in)
    })
    
    output$color_theme_indv_insig <- renderUI({
      ns <- session$ns
      # validate(need(a_file_pulldown_r()  != '', ""))
      # label = (HTML(paste(c('Colors for ',monitor_significance_thresholds()$sig, 'and', monitor_logfc_threshold()$sig))))
      label = (HTML('Colors for insignificant interactors'))
      colourpicker::colourInput(ns('color_indv_insig_in'), label, 
                                value = insigColorServer(), showColour = 'both', 
                                palette = c( "limited"), 
                                allowedCols = allowed_colors)
    })
    observeEvent(input$color_indv_insig_in, {
      insigColorServer(input$color_indv_insig_in)
    })
  })
}