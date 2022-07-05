summaryBox <- function(id) {
  box(
    title = "Summary", width = NULL, solidHeader = TRUE, status = "primary", 
    collapsible = TRUE, collapsed = FALSE,
    fluidRow(
      column(12, uiOutput(NS(id, "threshold_text"))),
      br(),
      column(12, tableOutput(NS(id, "verbatim_count_text"))),
      br(),
      column(12, uiOutput(NS(id, 'replicate_table_header'))),
      column(12, tableOutput(NS(id, "replicate_summary_table"))),
      br(),
      column(12, uiOutput(NS(id, 'sample_table_header'))),
      column(12, tableOutput(NS(id, "sample_summary_table"))),
      br(),
      column(12, uiOutput(NS(id, 'control_table_header'))),
      column(12, tableOutput(NS(id, "control_summary_table"))),
      br(),
      column(12, uiOutput(NS(id, 'sample_control_table_header'))),
      column(12, tableOutput(NS(id, "sample_control_summary_table"))),
      br(),
    ),
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

# works with output from enumerate_replicate_combinations, refer to this function 
#   in the Genoppi package for details of the combination_vec
parse_combinations <- function(df) {
  # assumption for combintation_vec:
  #   * rep only combine with other rep (e.g., rep1.rep2)
  #   * sample and control combines with its own type or the other (e.g.,
  #     sample1.sample2, control1.control2, sample1.control2)
  #   * all elements must be one of the above described combinations
  combinations <- enumerate_replicate_combinations(df)
  req(length(combinations) > 0)
  combination_list <- lapply(combinations, function(combo) {
    combo_vec <- 
      c(gsub("\\.", " vs ", combo), # annotation text for display
      gsub("\\.[^\\.]+$", "", combo), # first part of combo (things after . removed)
      gsub("^[^\\.]+\\.", "", combo)) # second part of combo (things before . removed)
    if (grepl("rep", combo)) {
      combo_vec <- c(combo_vec, "rep")}
    else if (grepl("sample", combo) & !grepl("control", combo)) {
      combo_vec <- c(combo_vec, "sample")}
    else if (!grepl("sample", combo) & grepl("control", combo)) {
      combo_vec <- c(combo_vec, "control")}
    else if (grepl("sample", combo) & grepl("control", combo)) {
      combo_vec <- c(combo_vec, "sample_control")}
    else {stop(
    "unexpected column in data frame supplied to parse_combinations
           (helper function for summaryStatsServer).")}
  })
  
  # organize processed data into a summary data frame 
  summary_df <- data.frame(do.call(rbind, combination_list))
  names(summary_df) <- c("Comparison", "first", "second", "type")
  
  # calculate correlations
  summary_df$cor <- apply(summary_df, 1, function(row){
    first_column <- row["first"]
    second_column <- row["second"]
    stats::cor(df[,first_column], df[,second_column])
  })
  summary_df["Correlation (r)"] <- apply(summary_df, 1, function(row)
    format(as.numeric(row["cor"]), digits = 4)
  )
  return(summary_df)
}

summaryStatsServer <- function(id, enrichmentStatsServer) {
  if (!is.reactive(enrichmentStatsServer)){
    stop("enrichmentStatsServer passed to summaryStatsServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    eventReactive(enrichmentStatsServer(), {
      df <- enrichmentStatsServer()
      # render the summary text for the data frame:
      #   (number of significant proteins out of all proteins)
      req(!is.null(df$significant))
      signifCount <- paste(bold(sum(df$significant)), "out of",
                         bold(nrow(df)), 'proteins significant.')
      
      # render the summary table for the data frame: (correlation between --
      #   replicates, samples, controls, sample & control, and their averages)
      corrTable <- parse_combinations(df)
      
      # calculate average of correlations for each type, i.e.:
      #   rep vs rep
      #   sample vs sample
      #   control vs control
      #   sample vs control
      #   sample vs sample & control vs control & sample vs control,
      #     (all combinations except rep vs rep)
      types <- unique(corrTable$type)
      type_dfs_list <- lapply(types, function(typ) {
        type_entries <- corrTable[corrTable["type"] == typ, ]
        type_count <- dim(type_entries)[1]
        # no need to handle case of zero, type_count > 0
        type_avg <- sum(type_entries["cor"])/type_count
        type_avg_formatted <- format(as.numeric(type_avg), digits = 4)
        type_text <- gsub("_", " vs ", typ)
        avg_row <- c(paste(type_text, "average"),   # - Comparison
                     "NA", "NA", "NA",              # - first, second, type
                     type_avg, type_avg_formatted)  # - cor, Correlation (r)
        return(rbind(type_entries, avg_row)[, c("Comparison", "Correlation (r)")])
      })
      names(type_dfs_list) <- types
      return(list(
        signifCountText=signifCount, 
        corrTableList=type_dfs_list))
    })
  })
}

thresholdTextServer <- function(id, statsParamsServer) {
  if (!is.reactive(statsParamsServer)){
    stop("statsParamsServer passed to thresholdTextServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    eventReactive(statsParamsServer(), {
      req(statsParamsServer()$signifType)
      # render text for showing significance threshold (FDR/P-Value)
      if (statsParamsServer()$signifType == 'fdr'){
        signifText <- paste('FDR ≤', statsParamsServer()$fdrThresh)
      } else if (statsParamsServer()$signifType == 'pvalue') {
        signifText <- paste('<i>P</i>-value ≤', statsParamsServer()$pValThresh)
      } else {
        stop("Invalid signifType from statsParamsServer supplied to thresholdTextServer.")
      }
      
      # render text for showing logFC threshold
      if (statsParamsServer()$logfcDir == 'negative') {
        fcText <- paste(
          "log<sub>2</sub>FC&lt;", -statsParamsServer()$logfcThresh)
      } else if (statsParamsServer()$logfcDir == 'positive') {
        fcText <- paste(
          "log<sub>2</sub>FC&ge;", statsParamsServer()$logfcThresh)
      } else {
        if (statsParamsServer()$logfcDir != 'both') {
          stop("Invalid logfcDir in thresholdTextServer")}
        fcText <- paste(
          "|log<sub>2</sub>FC|&ge;", statsParamsServer()$logfcThresh)
      }
      thresholdText <- paste(
        "Significance threshold:", signifText, 'and', fcText)
      return(thresholdText)
    })
  })
}

summaryDisplayServer <- function(id, 
                                 summaryStatsServer,
                                 thresholdTextServer) {
  if (!is.reactive(summaryStatsServer)){
    stop("summaryStatsServer passed to summaryDisplayServer is not reactive")}
  if (!is.reactive(thresholdTextServer)){
    stop("thresholdTextServer passed to summaryDisplayServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    output$threshold_text <- renderUI({
      HTML(thresholdTextServer())
      })
    output$verbatim_count_text <- renderUI({
      HTML(summaryStatsServer()$signifCountText)
    })
    output$replicate_table_header <- renderUI({
      h5(HTML(bold("Replicate correlation(s):")))
    })
    output$replicate_summary_table <- renderTable({
      summaryStatsServer()$corrTableList$rep
    })
    output$sample_table_header <- renderUI({
      h5(HTML(bold("Sample correlation(s):")))
    })
    output$sample_summary_table <- renderTable({
      summaryStatsServer()$corrTableList$sample
    })
    output$control_table_header <- renderUI({
      h5(HTML(bold("Control correlation(s):")))
    })
    output$control_summary_table <- renderTable({
      summaryStatsServer()$corrTableList$control
    })
    output$sample_control_table_header <- renderUI({
      h5(HTML(bold("Sample vs Control correlation(s):")))
    })
    output$sample_control_summary_table <- renderTable({
      summaryStatsServer()$corrTableList$sample_control
    })
  })
}