volcanoPlotServer <- function(id) {
  moduleServer(id, function(input, output, session){
    return(reactiveVal(value=NULL))})
}

overlaidVolcanoPlotServer <- function(id) {
  moduleServer(id, function(input, output, session){
    return(reactiveVal(value=NULL))})
}


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
                              volcanoPlotServer,
                              enrichmentStatsServer,
                              baitServer,
                              goiServer,
                              sigColorServer, 
                              insigColorServer) {
  if (!is.reactive(enrichmentStatsServer)){
    stop("enrichmentStatsServer passed to drawVolcanoServer is not reactive")}
  if (!is.reactive(baitServer)){
    stop("baitServer passed to drawVolcanoServer is not reactive")}
  if (!is.reactive(goiServer)){
    stop("goiServer passed to drawVolcanoServer is not reactive")}
  if (!is.reactive(sigColorServer)){
    stop("sigColorServer passed to drawVolcanoServer is not reactive")}
  if (!is.reactive(insigColorServer)){
    stop("insigColorServer passed to drawVolcanoServer is not reactive")}
  moduleServer(id, function(input, output, session){
    observeEvent({
      enrichmentStatsServer()
      baitServer()
      sigColorServer()
      insigColorServer()}, 
      {
        req(enrichmentStatsServer())
        df <- enrichmentStatsServer()
        col_significant <- "#41AB5D"
        if(!is.null(sigColorServer())){col_significant <- sigColorServer()}
        col_insignificant <- "#808080"
        if(!is.null(insigColorServer())){col_insignificant <- insigColorServer()}
        p <- plot_volcano_basic(
          df, col_significant = col_significant, col_other = col_insignificant)
        # if (!is.null(baitServer())) {
        #   p <- plot_overlay(p, as.bait(baitServer())) # add bait
        # }
        # p <- make_interactive(p, legend = T)
        # output$VolcanoPlot <- plotly::renderPlotly({
        #   # validate(need(a_file_pulldown_r()  != '', "Upload file"))
        #   p
        #   # a_vp_layerx() #TODO
        # })
        volcanoPlotServer(p)
      }
    )
  })
}
# TODO overlay goi
overlayVolcanoServer <- function(id, 
                                 volcanoPlotServer,
                                 baitServer,
                                 goiServer) {
  if (!is.reactive(baitServer)){
    stop("baitServer passed to drawVolcanoServer is not reactive")}
  if (!is.reactive(goiServer)){
    stop("goiServer passed to drawVolcanoServer is not reactive")}
  moduleServer(id, function(input, output, session){
    observeEvent(c(volcanoPlotServer(),baitServer()), {
        req(volcanoPlotServer())
        p <- volcanoPlotServer()
        if (!is.null(baitServer())) {
          p <- plot_overlay(p, as.bait(baitServer())) # add bait
        }
        p <- make_interactive(p, legend = T)
        output$VolcanoPlot <- plotly::renderPlotly({
          # validate(need(a_file_pulldown_r()  != '', "Upload file"))
          p
          # a_vp_layerx() #TODO
        })
      }
    )
  })
}



