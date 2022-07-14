plotGGpairFrame <- function(id) {
  box(
    title = tagList(img(src='icon_scatter.png',width='22px'), "GGPairs"), width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = FALSE,
    #fluidRow(
    #  column(11, shinyjs::hidden(myDownloadButton("a_scatter_plot_download", 'Scatter plot')))
    #),
    fluidRow(style = "padding-bottom:75px",
             column(11, shinycssloaders::withSpinner(plotOutput(NS(id, "GGPairs")), spinner_type)) #, width = "550px", height = "550px"
    )
  )
}

plotGGpairServer <- function(id, dataFrameServer, extractColumnsServer) {
  moduleServer(id, function(input, output, session){
    observeEvent(dataFrameServer(), {
      output$GGPairs <- renderPlot(plot_replicate_ggpair(dataFrameServer()$data, extractColumnsServer())) #TODO replace column with variable
    })
  })
}