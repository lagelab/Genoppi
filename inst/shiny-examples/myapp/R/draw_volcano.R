sigColorServer <- function(id){
  moduleServer(id, function(input, output, session){
    return(reactiveVal(value='#41AB5D'))})
}

insigColorServer <- function(id){
  moduleServer(id, function(input, output, session){
    return(reactiveVal(value='#808080'))})
}

drawVolcanoPlot <- function(id) {
  box(
    title = tagList(shiny::icon('chart-area'), "Volcano plot"), width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE,
    # fluidRow(
    #   column(11, shinyjs::hidden(myDownloadButton("a_volcano_plot_download", 'Volcano plot')))
    # ),
    fluidRow(style = "padding-bottom:75px",
             #column(1, plotOutput("FDR_colorbar", width = "50px")),
             column(12, shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, "VolcanoPlot")), spinner_type)) #, width = "550px", height = "550px"
    ),
  )
}

drawVolcanoServer <- function(id, 
                              enrichmentStatsServer, 
                              sigColorServer, 
                              insigColorServer) {
  if (!is.reactive(enrichmentStatsServer)){
    stop("enrichmentStatsServer passed to drawVolcanoServer is not reactive")}
  if (!is.reactive(sigColorServer)){
    stop("sigColorServer passed to drawVolcanoServer is not reactive")}
  if (!is.reactive(insigColorServer)){
    stop("insigColorServer passed to drawVolcanoServer is not reactive")}
  moduleServer(id, function(input, output, session){
    observeEvent({c(enrichmentStatsServer(), 
                    sigColorServer(), 
                    insigColorServer())}, {
      req(enrichmentStatsServer(), 
             sigColorServer(), 
             insigColorServer())
      df <- enrichmentStatsServer()
      p <- plot_volcano_basic(df, 
                              col_significant = sigColorServer(), 
                              col_other = insigColorServer()) %>% 
        make_interactive(legend = T)
      output$VolcanoPlot <- plotly::renderPlotly({
        # validate(need(a_file_pulldown_r()  != '', "Upload file"))
        p
        # a_vp_layerx() #TODO
      })
    })
  })
}