plotValues <- function(id) {
  moduleServer(id, function(input, output, session){
    return(reactiveValues())})
}

# depracated, replaced with plotValues
# volcanoPlotServer <- function(id) {
#   moduleServer(id, function(input, output, session){
#     return(reactiveVal(value=NULL))})
# }

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
    title = tagList(shiny::icon('chart-area'), "Volcano plot"),
    width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE,
    fluidRow(
      column(12, shinyjs::hidden(myDownloadButton(
        NS(id, "a_volcano_plot_download"), 'Volcano plot')))
    ),
    fluidRow(style = "padding-bottom:75px",
             # column(1, plotOutput(NS(id, "FDRColorBar"), width = "50px")),
             column(12, shinycssloaders::withSpinner(
               # plotOutput(NS(id, "vp"))))
               plotly::plotlyOutput(NS(id, "VolcanoPlot")), spinner_type)) #, width = "550px", height = "550px"
    ),
  )
}

drawVolcanoServer <- function(id, 
                              plotValues,
                              sigificanceServer,
                              sigColorServer, 
                              insigColorServer) {
  if (!is.reactivevalues(plotValues)){
    stop("plotValues passed to drawVolcanoServer is not reactiveValues")}
  if (!is.reactive(sigificanceServer)){
    stop("sigificanceServer passed to drawVolcanoServer is not reactive")}
  if (!is.reactive(sigColorServer)){
    stop("sigColorServer passed to drawVolcanoServer is not reactive")}
  if (!is.reactive(insigColorServer)){
    stop("insigColorServer passed to drawVolcanoServer is not reactive")}
  moduleServer(id, function(input, output, session){
    observeEvent(is.null(plotValues$volcano_basic),{
      shinyjs::hide("a_volcano_plot_download")})
    observeEvent(ignoreInit = T,
                 !is.null(plotValues$volcano_basic),
                 {shinyjs::show("a_volcano_plot_download")})
    output$a_volcano_plot_download = downloadHandler(
      filename = 'genoppi-volcano-plot.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        ggsave(file, plot =  theme_volcano(plotValues$volcano_basic), 
               device = device, 
               width = global.img.volcano.download.width,
               height = global.img.volcano.download.height)
      })
    
    observeEvent({
      sigificanceServer()
      sigColorServer()
      insigColorServer()}, 
      {
        req(sigificanceServer())
        df <- sigificanceServer()
        col_significant <- "#41AB5D"
        if(!is.null(sigColorServer())){col_significant <- sigColorServer()}
        col_insignificant <- "#808080"
        if(!is.null(insigColorServer())){col_insignificant <- insigColorServer()}
        p <- plot_volcano_basic(
          df, col_significant = col_significant, col_other = col_insignificant)
        plotValues$volcano_basic <- p
      }
    )
  })
}

overlayVolcanoServer <- function(id, 
                                 plotValues,
                                 baitServer,
                                 goiServer,
                                 goiAlphaServer,
                                 statsParamsValues) {
  if (!is.reactivevalues(plotValues)){
    stop("plotValues passed to overlayVolcanoServer is not reactiveValues")}
  if (!is.reactive(baitServer)){
    stop("baitServer passed to overlayVolcanoServer is not reactive")}
  if (!is.reactive(goiServer)){
    stop("goiServer passed to overlayVolcanoServer is not reactive")}
  if (!is.reactive(goiAlphaServer)){
    stop("goiAlphaServer passed to overlayVolcanoServer is not reactive")}
  if (!is.reactivevalues(statsParamsValues)){
    stop("statsParamsValues passed to overlayVolcanoServer is not reactive")}
  moduleServer(id, function(input, output, session){
    observeEvent(
      c(plotValues$volcano_basic,
        baitServer()), 
      {
        p <- plotValues$volcano_basic
        if (!is.null(baitServer())) {
          p <- plot_overlay(p, as.bait(baitServer()))} # add bait
        plotValues$volcano_bait_overlay <- p
      }
    )
    observeEvent(
      c(plotValues$volcano_bait_overlay,
        # baitServer(),
        goiServer(),
        goiAlphaServer(),
        statsParamsValues$signifType,
        statsParamsValues$pValThresh,
        statsParamsValues$logfcThresh,
        statsParamsValues$logfcDir), 
      {
        req(plotValues$volcano_bait_overlay)
        p <- plotValues$volcano_bait_overlay
        # if (!is.null(baitServer())) {
        #   p <- plot_overlay(p, as.bait(baitServer())) # add bait
        # }
        output$vp <- renderPlot(p)
        p <- make_interactive(p, legend = T)
        if (!is.null(goiServer())) {
          p <- add_plotly_markers_search(
            p, goiServer(), alpha = goiAlphaServer()) 
        }
        req(statsParamsValues$logfcThresh,
            statsParamsValues$logfcDir,
            statsParamsValues$signifType)
        if (statsParamsValues$signifType == "pvalue") {
          req(statsParamsValues$pValThresh)
        }
        p <- genoppi::add_plotly_threshold_lines (
          p,
          line_pvalue = statsParamsValues$pValThresh, 
          line_logfc = statsParamsValues$logfcThresh, 
          logfc_direction = statsParamsValues$logfcDir, 
          sig_type = statsParamsValues$signifType
        )
        # p <- add_plotly_layout_volcano(
        #   p, 
        #   width = global.basic.volcano.width, 
        #   height = global.basic.volcano.height
        # )
        output$VolcanoPlot <- plotly::renderPlotly({
          # validate(need(a_file_pulldown_r()  != '', "Upload file"))
          p
        })
      }
    )
  })
}



