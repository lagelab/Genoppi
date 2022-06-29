summaryBox <- function(id) {
  box(
    title = "Summary", width = NULL, solidHeader = TRUE, status = "primary", 
    collapsible = TRUE, collapsed = FALSE,
    fluidRow(
      column(12, uiOutput(NS(id, "VP_count_text"))),
      br(),
    ),
    fluidRow(
      column(12, tableOutput(NS(id, "verbatim_count_ui"))),
      br()
    ),
    fluidRow(
      br(),
      column(12, uiOutput(NS(id, 'replicate_summar_text_ui'))),
    ),
    fluidRow(
      column(12, tableOutput(NS(id, "replicate_summary_table_ui")))
    ),
    # br(),
    # fluidRow(
    #   column(12, uiOutput(NS(id, "monitor_pulldown_ui")))
    # ),
    # fluidRow(
    #   column(12, uiOutput(NS(id, "monitor_pulldown_mapping_ui")))
    # ),
    # fluidRow(
    #   br(),
    #   column(12, shinyjs::hidden(myDownloadButton("a_mttest_mapping_download", 'Proteomic data', icon("download"))))
    # )
  )
}

volcanoPlotStatsServer <- function(id, dataServer) { #check which dataServer
  moduleServer(id, function(input, output, session) {
  })
}

summaryDisplayServer <- function(id, volcanoPlotStatsServer) {
  moduleServer(id, function(input, output, session) {
  })
}