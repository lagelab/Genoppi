plotGGpairFrame <- function(id) {
  box(
    title = tagList(img(src='icon_scatter.png',width='22px'), "Pairwise scatter plot"),
    width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE, 
    collapsed = FALSE,
    fluidRow(
     column(11, shinyjs::hidden(
       myDownloadButton(NS(id, "GGPair_download"), 'GGPairs Plot')))
    ),
    fluidRow(#style = "padding-bottom:75px",
             # TODO add spinner to ggpairs
             #  (low priority as volcano plot already have spinner)
             # column(11, shinycssloaders::withSpinner(
             #   plotOutput(NS(id, "GGPairs")), spinner_type)) #, width = "550px", height = "550px"
             column(11, actionButton(NS(id, "generatePlot"), "Generate plot")),
             column(11, plotOutput(NS(id, "GGPairs"))) 
    ),
    fluidRow(column(11, shinyjs::hidden(uiOutput(NS(id, "GGPairsNotif")))))
  )
}

plotGGpairServer <- function(id, 
                             sigificanceServer, 
                             columnsValues,
                             plotValues,
                             sigColorServer, 
                             insigColorServer) {
  moduleServer(id, function(input, output, session){
    # observeEvent(sigificanceServer(), {
    observeEvent(input$generatePlot, {
      req(columnsValues$mod_ttest_columns,
          sigColorServer(),
          insigColorServer())
      output$GGPairs <- renderPlot({
        plotValues$ggpairs_plot <- plot_replicate_ggpair(
          sigificanceServer(),
          unlist(columnsValues$mod_ttest_columns),
          overlay_sigificance=TRUE,
          significant_color=sigColorServer(),
          insignificant_color=insigColorServer())
        plotValues$ggpairs_plot
        })
      })
    
    output$GGPair_download <- downloadHandler(
      filename = 'genoppi-ggpairs-plot.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        ggplot2::ggsave(file,
               plot =  plotValues$ggpairs_plot,
               device = device,
               width = global.img.volcano.download.width,
               height = global.img.volcano.download.height)
      })
    
    output$GGPairsNotif <- renderUI({HTML(
      "No columns used for calculating enrichment statistics."
    )})
    
    observeEvent(
      columnsValues$mod_ttest_columns,
      {
        if (length(columnsValues$mod_ttest_columns) > 0) {
          shinyjs::show("GGPairs")
          shinyjs::show("GGPair_download")
          shinyjs::hide("GGPairsNotif")
        } else {
          shinyjs::hide("GGPairs")
          shinyjs::hide("GGPair_download")
          shinyjs::show("GGPairsNotif")
        }
    })
  })
}