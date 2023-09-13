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

summaryStatsServer <- function(id, sigificanceServer) {
  if (!is.reactive(sigificanceServer)){
    stop("sigificanceServer passed to summaryStatsServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    eventReactive(sigificanceServer(), {
      df <- sigificanceServer()
      # render the summary text for the data frame:
      #   (number of significant proteins out of all proteins)
      req(!is.null(df$significant))
      signifCount <- paste(genoppi::bold(sum(df$significant)), "out of",
                           genoppi::bold(nrow(df)), 'proteins significant.')
      
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
        if (!grepl(" vs ", type_text)) {
          type_text <- paste(type_text, "vs", type_text)}
        return(c(type_text, type_avg_formatted))
        # # USE following for verbose correlation table 
        # #   (i.e., correlation for every comparison)
        # avg_row <- c(paste(type_text, "average"),   # - Comparison
        #              "NA", "NA", "NA",              # - first, second, type
        #              type_avg, type_avg_formatted)  # - cor, Correlation (r)
        # return(rbind(type_entries, avg_row)[, c("Comparison", "Correlation (r)")])
      })
      names(type_dfs_list) <- types
      
      avg_df <- data.frame(do.call(rbind, type_dfs_list))
      names(avg_df) <- c("Comparison", "Mean correlation (r)")
      rownames(avg_df) <- avg_df$Comparison
      
      return(list(
        signifCountText=signifCount, 
        # # UNCOMMENT for verbose table (i.e., correlation for every comparison)
        # corrTableList=type_dfs_list,
        avgCorrTable=avg_df))
    })
  })
}

thresholdsValues <- function(id) {
  moduleServer(id, function(input, output, session){
    return(reactiveValues())})
}

thresholdTextServer <- function(id, statsParamsValues, thresholdsValues) {
  if (!is.reactivevalues(statsParamsValues)){
    stop("statsParamsValues passed to thresholdTextServer is not reactive")}
  if (!is.reactivevalues(thresholdsValues)){
    stop("thresholdsValues passed to thresholdTextServer is not reactive values")}
  moduleServer(id, function(input, output, session) {
    observeEvent(c(statsParamsValues$signifType,
                   statsParamsValues$fdrThresh,
                   statsParamsValues$pValThresh,
                   statsParamsValues$logfcThresh,
                   statsParamsValues$logfcDir), {
      req(statsParamsValues$signifType)
      # render text for showing significance threshold (FDR/P-Value)
      sigType <- statsParamsValues$signifType
      if (sigType == 'fdr'){
        sigVal <-statsParamsValues$fdrThresh
        sigTxt <- paste0('FDR≤', sigVal)
        insigTxt <- paste0('FDR>', sigVal)
      } else if (sigType == 'pvalue') {
        sigVal <-statsParamsValues$pValThresh
        sigTxt <- paste0('<i>P</i>-value≤', sigVal)
        insigTxt <- paste0('<i>P</i>-value>', sigVal)
      } else {
        stop("Invalid signifType from statsParamsValues supplied to thresholdTextServer.")
      }
      
      # render text for showing logFC threshold
      fc <- statsParamsValues$logfcThresh
      fcDir <- statsParamsValues$logfcDir
      if (fcDir == 'negative') {
        fcSigTxt <- paste("log<sub>2</sub>FC&lt;", fc)
        fcInsigTxt <- paste("log<sub>2</sub>FC&ge;", -fc)
      } else if (fcDir == 'positive') {
        fcSigTxt <- paste("log<sub>2</sub>FC&ge;", fc)
        fcInsigTxt =  paste("log<sub>2</sub>FC&lt;", fc)
      } else {
        if(fcDir != 'both'){stop("Invalid logfcDir in thresholdTextServer")}
        fcSigTxt <- paste("|log<sub>2</sub>FC|&ge;", fc)
        fcInsigTxt <- paste("|log<sub>2</sub>FC|&lt;", fc)
      }
      summary <- paste("Significance threshold:",sigTxt,'and',fcSigTxt)
      
      # Set passed in reactiveValues
      thresholdsValues$summary <- summary
      thresholdsValues$sigType <- sigType
      thresholdsValues$sigTxt <- sigTxt
      thresholdsValues$insigTxt <- insigTxt
      thresholdsValues$sigVal <- sigVal
      thresholdsValues$fcSigTxt <- fcSigTxt
      thresholdsValues$fcInsigTxt <- fcInsigTxt
    })
  })
}


summaryBox <- function(id) {
  box(
    title = "Summary", width = NULL, solidHeader = TRUE, status = "primary", 
    collapsible = TRUE, collapsed = FALSE,
    fluidRow(
      column(12, uiOutput(NS(id, "threshold_text"))),
      br(),
      column(12, tableOutput(NS(id, "verbatim_count_text"))),
      br(),
      column(12, tableOutput(NS(id, "correlation_table_header"))),
      br(),
      column(12, uiOutput(NS(id, "correlation_table"))),
      br(),
      # # UNCOMMENT for verbose table (i.e., correlation for every comparison)
      # column(12, uiOutput(NS(id, 'replicate_table_header'))),
      # column(12, tableOutput(NS(id, "replicate_summary_table"))),
      # br(),
      # column(12, uiOutput(NS(id, 'sample_table_header'))),
      # column(12, tableOutput(NS(id, "sample_summary_table"))),
      # br(),
      # column(12, uiOutput(NS(id, 'control_table_header'))),
      # column(12, tableOutput(NS(id, "control_summary_table"))),
      # br(),
      # column(12, uiOutput(NS(id, 'sample_control_table_header'))),
      # column(12, tableOutput(NS(id, "sample_control_summary_table"))),
      # br(),
    ),
    fluidRow(
      br(),
      column(12, uiOutput(NS(id, "input_format_err_text")))
    ),
    fluidRow(
      br(),
      column(12, uiOutput(NS(id, "gene_or_mapping_err_text")))
    ),
    fluidRow(
      br(),
      column(12, uiOutput(NS(id, "stats_err_text")))
    ),
    fluidRow(
      br(),
      column(12, shinyjs::hidden(
        myDownloadButton(
          NS(id, "download_dataframe_with_stats"),
          'Proteomic data', icon("download"))))
    )
  )
}
summaryDisplayServer <- function(id, 
                                 summaryStatsServer,
                                 statsParamsValues,
                                 thresholdsValues,
                                 significanceServer,
                                 columnsValues,
                                 errorValues) {
  if (!is.reactive(summaryStatsServer)){
    stop("summaryStatsServer passed to summaryDisplayServer is not reactive")}
  if (!is.reactivevalues(statsParamsValues)){
    stop("statsParamsValues passed to summaryDisplayServer is not reactive")}
  if (!is.reactivevalues(thresholdsValues)){
    stop("thresholdsValues passed to summaryDisplayServer is not reactive values")}
  moduleServer(id, function(input, output, session) {
    observeEvent(thresholdsValues$summary, {
      output$threshold_text <- renderUI({
        req(thresholdsValues$summary)
        HTML(thresholdsValues$summary)
        })
    })
    output$verbatim_count_text <- renderUI({
      HTML(summaryStatsServer()$signifCountText)
    })
    output$correlation_table_header <- renderUI({
      h5(HTML(genoppi::bold("Sample correlation(s):")))
    })
    
    observeEvent(
      # c(summaryStatsServer()$avgCorrTable, columnsValues$mod_ttest_columns), 
      # columnsValues$mod_ttest_columns, 
      significanceServer(),
      {
        req(columnsValues$mod_ttest_columns)
        if (length(columnsValues$mod_ttest_columns)==0) {
          output$correlation_table <- renderUI({
            HTML("No columns used for calculating enrichment statistics.")
          })
        }
        else {
          avgTable <- summaryStatsServer()$avgCorrTable
          if (any(grepl("^sample[0-9]$", columnsValues$mod_ttest_columns)) & 
              any(grepl("^control[0-9]$", columnsValues$mod_ttest_columns))) {
            output$correlation_table <- renderTable({
              avgTable[
                c("sample vs sample", "control vs control", "sample vs control"),]
            })
          } 
          else if (any(grepl("^rep[0-9]$", columnsValues$mod_ttest_columns))) {
            output$correlation_table <- renderTable({
              avgTable[c("rep vs rep"),]
            })
          }
          else {
            print("columnsValues$mod_ttest_columns value:")
            print(columnsValues$mod_ttest_columns)
            stop(
            "Invalid columnsValues$mod_ttest_columns value found in summaryDisplayServer")
          }
        }
    })
    
    output$input_format_err_text <- renderUI({
      req(errorValues$input_errors)
      HTML(errorValues$input_errors)
    })
    
    output$gene_or_mapping_err_text <- renderUI({
      if (!is.null(errorValues$gene_symbol_error)) {
        # display user messages if using gene column directly
        HTML(errorValues$gene_symbol_error)
      } else if (!is.null(errorValues$mapping_error)) {
        # display user messages if using mapAccessionToGeneServer with accession_number column
        HTML(errorValues$mapping_error)
      }
    })
    output$stats_err_text <- renderUI({
      req(errorValues$stats_errors)
      HTML(errorValues$stats_errors)
    })
    
    output$download_dataframe_with_stats <- downloadHandler(
      # TODO Unify download handlers
      filename = function() {
        paste0("genoppi-proteomic-results",".txt")
      },
      content = function(file) {
        write.table(significanceServer(), file, row.names = F, sep="\t")
      }
    )
    observeEvent(significanceServer(), {
      shinyjs::toggle(
        id="download_dataframe_with_stats", 
        condition=!is.null(significanceServer()))
    })
    
    # # UNCOMMENT for verbose table (i.e., correlation for every comparison)
    # output$replicate_table_header <- renderUI({
    #   h5(HTML(genoppi::bold("Replicate correlation(s):")))
    # })
    # output$replicate_summary_table <- renderTable({
    #   summaryStatsServer()$corrTableList$rep
    # })
    # output$sample_table_header <- renderUI({
    #   h5(HTML(genoppi::bold("Sample correlation(s):")))
    # })
    # output$sample_summary_table <- renderTable({
    #   summaryStatsServer()$corrTableList$sample
    # })
    # output$control_table_header <- renderUI({
    #   h5(HTML(genoppi::bold("Control correlation(s):")))
    # })
    # output$control_summary_table <- renderTable({
    #   summaryStatsServer()$corrTableList$control
    # })
    # output$sample_control_table_header <- renderUI({
    #   h5(HTML(genoppi::bold("Sample vs Control correlation(s):")))
    # })
    # output$sample_control_summary_table <- renderTable({
    #   summaryStatsServer()$corrTableList$sample_control
    # })
  })
}