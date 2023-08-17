# shiny server
shinyServer(function(input, output, session){
  #suppress warnings
  storeWarn<- getOption("warn")
  options(warn = -1) 
  
  observe({
    input$filetype
    updateTabsetPanel(session, "basic", selected = "p1")
  })
  
  ##### VISUALIZATIONS START ##### 
  
  ## ARCHIVE
  # 
  # documentation
  # output$info_inweb_ui <- renderUI({actionLink('info_inweb', ' ', icon = icon('question-circle'))})
  # observeEvent(input$info_inweb,{
  #   text = paste(readLines('documentation/inweb_table.info'))
  #   
  #   text = paste(readLines(find_docs('inweb_table.info')), '<br> <br>', 
  #                readLines(find_docs('irefindex_table.info')), '<br> <br>', 
  #                readLines(find_docs('bioplex_table.info')))
  #   showModal(modalDialog(HTML(text), easyClose = T, footer=genoppi.ver, title = 'InWeb'))
  # })
  ######
  # documentation tab
  output$documentation_ui <- renderText({
    return(HTML(documentation))
  })
  
  # hide developer tabs when starting app
  hideTab('basic','p5')
  
  developer <- reactiveVal(value = FALSE)
  observeEvent(input$enable_dev_mode,{
    showTab('basic','p5')
    developer(TRUE)
  })
  
  output$a_example_file_ui <- renderUI({
    fileInput('a_file_pulldown_extra', 'Upload a proteomic data file to get started!', accept = files_accepted)
  })
  
  output$a_get_example_file_ui <- renderUI({
    # HTML(paste('Try a single',actionLink('a_get_example_file', 'example file'), 'or',
    HTML(paste(actionLink('a_get_example_multiple_files', 'multiple example files')))
  })
  
  observeEvent(input$a_get_example_file,{
    updateTabItems(session, "sidebarmenu", 'dashboard')
    file1_path(example_file)
    dataPath(example_file)
  })
  
  observeEvent(input$a_file_pulldown_extra,{
    updateTabItems(session, "sidebarmenu", 'dashboard')
    file1_path(input$a_file_pulldown_extra$datapath)
  })
  
  observeEvent(input$tab_welcome, {
    updateTabItems(session, "sidebarmenu", 'guide')
  })
  
  observeEvent(input$a_file_pulldown_r,{
    file1_path(input$a_file_pulldown_r$datapath)
  })
  
  file1_path <- reactiveVal(value = NULL)
  
  a_file_pulldown_r <- reactive({
    validate(need(!is.null(file1_path()), ''))
    data.frame(datapath = file1_path(), stringsAsFactors = F)
  })
  
  
  output$a_color_scheme <- renderUI({
    radioButtons("colorscheme", "Color scheme:",
                 c("FDR" = "fdr", 
                   "ExAC" = "exac",
                   "Grayscale" = "cbf",
                   "User value" = "user"),
                 inline = T)
  })
  
  output$a_sig_text_ui <- renderUI({
    req(thldVals$sigTxt, thldVals$fcSigTxt)
    HTML(paste0(thldVals$sigTxt, ', ', thldVals$fcSigTxt))
  })
  
  output$a_insig_text_ui <- renderUI({
    req(thldVals$insigTxt, thldVals$fcInsigTxt)
    HTML(paste0(thldVals$insigTxt, ', ',thldVals$fcInsigTxt))
  })
  
  #####################
  # Multiple file tab #
  #####################
  
  multiDataS1 <- dataServer("multi1")
  multiDataS2 <- dataServer("multi2")
  multiDataS3 <- dataServer("multi3")
  
  # Servers for toggling different panels  
  multiFilePanelUpdate <- triggerPanelUpdateServer("multi_file")
  
  # Servers for storing genes
  multi_baitS1 <- baitServer("gene_overlay1")
  multi_baitS2 <- baitServer("gene_overlay2")
  multi_baitS3 <- baitServer("gene_overlay3")
  multi_goiS1 <- goiServer("gene_overlay1")
  multi_goiS2 <- goiServer("gene_overlay2")
  multi_goiS3 <- goiServer("gene_overlay3")
  multi_goiAlphaS1 <- goiAlphaServer("gene_overlay1")
  multi_goiAlphaS2 <- goiAlphaServer("gene_overlay2")
  multi_goiAlphaS3 <- goiAlphaServer("gene_overlay3")
  
  # Servers for storing plots
  multi_sharedPlotVals <- plotValues("multi_shared")
  multi_plotVals1 <- plotValues("multi_volcano_plot1")
  multi_plotVals2 <- plotValues("multi_volcano_plot2")
  multi_plotVals3 <- plotValues("multi_volcano_plot3")
  
  # initialize reactive values for plot options
  multi_statsParamVals1 <- statsParamsValues("multi_stats_input1")
  multi_statsParamVals2 <- statsParamsValues("multi_stats_input2")
  multi_statsParamVals3 <- statsParamsValues("multi_stats_input3")
  multi_sigColorS1 <- sigColorServer("multi_sig_color1")
  multi_sigColorS2 <- sigColorServer("multi_sig_color2")
  multi_sigColorS3 <- sigColorServer("multi_sig_color3")
  multi_insigColorS1 <- insigColorServer("multi_insig_color1")
  multi_insigColorS2 <- insigColorServer("multi_insig_color2")
  multi_insigColorS3 <- insigColorServer("multi_insig_color3")
  
  # Server for storing reactive error messages
  multi_errVals1 <- errorValues("error_messsages1")
  multi_errVals2 <- errorValues("error_messsages2")
  multi_errVals3 <- errorValues("error_messsages3")
  
  # Servers for reading data in multi file mode
  multi_sigS1 <- sigificanceServer("mutli1")
  multi_sigS2 <- sigificanceServer("mutli2")
  multi_sigS3 <- sigificanceServer("mutli3")
  multi_dataPathS1 <- dataPathServer("mutli1")
  multi_dataPathS2 <- dataPathServer("mutli2")
  multi_dataPathS3 <- dataPathServer("mutli3")
  multi_colVals1 <- columnsValues("mutli1")
  multi_colVals2 <- columnsValues("mutli2")
  multi_colVals3 <- columnsValues("mutli3")
  multi_dataFrameS1 <- dataFrameServer("mutli1", multi_dataPathS1)
  multi_dataFrameS2 <- dataFrameServer("mutli2", multi_dataPathS2)
  multi_dataFrameS3 <- dataFrameServer("mutli3", multi_dataPathS3)
  
  # Servers for processing data in multi file mode
  multi_inputErrS1 <- inputErrorServer("multi_data_check1", multi_dataFrameS1, multi_errVals1)
  multi_inputErrS2 <- inputErrorServer("multi_data_check2", multi_dataFrameS2, multi_errVals2)
  multi_inputErrS3 <- inputErrorServer("multi_data_check3", multi_dataFrameS3, multi_errVals3)
  multi_accMapErrS1 <- accessionMapErrorServer("multi_mapping_check1", multi_mapAccessionToGeneS1, multi_errVals1)
  multi_accMapErrS2 <- accessionMapErrorServer("multi_mapping_check2", multi_mapAccessionToGeneS2, multi_errVals2)
  multi_accMapErrS3 <- accessionMapErrorServer("multi_mapping_check3", multi_mapAccessionToGeneS3, multi_errVals3)
  multi_mapAccessionToGeneS1 <- mapAccessionToGeneServer("multi1", multi_dataFrameS1, multi_errVals1)
  multi_mapAccessionToGeneS2 <- mapAccessionToGeneServer("multi2", multi_dataFrameS2, multi_errVals2)
  multi_mapAccessionToGeneS3 <- mapAccessionToGeneServer("multi3", multi_dataFrameS3, multi_errVals3)
  multi_statsParamsS1 <- statsParamsServer("multi_stats_input1", multi_mapAccessionToGeneS1, multiDataS1, multi_colVals1, multi_statsParamVals1)
  multi_statsParamsS2 <- statsParamsServer("multi_stats_input2", multi_mapAccessionToGeneS2, multiDataS2, multi_colVals2, multi_statsParamVals2)
  multi_statsParamsS3 <- statsParamsServer("multi_stats_input3", multi_mapAccessionToGeneS3, multiDataS3, multi_colVals3, multi_statsParamVals3)
  multi_thldVals1 <- thresholdsValues("multi_file_thresholds1")
  multi_thldVals2 <- thresholdsValues("multi_file_thresholds2")
  multi_thldVals3 <- thresholdsValues("multi_file_thresholds3")
  multi_enrichmentStatsS1 <- enrichmentStatsServer("multi1", multi_mapAccessionToGeneS1, multi_statsParamVals1, multiDataS1, multi_errVals1)
  multi_enrichmentStatsS2 <- enrichmentStatsServer("multi2", multi_mapAccessionToGeneS2, multi_statsParamVals2, multiDataS2, multi_errVals2)
  multi_enrichmentStatsS3 <- enrichmentStatsServer("multi3", multi_mapAccessionToGeneS3, multi_statsParamVals3, multiDataS3, multi_errVals3)
  multi_findSigS1 <- findSignificantServer("multi1", multi_statsParamVals1, multiDataS1, multi_sigS1)
  multi_findSigS2 <- findSignificantServer("multi2", multi_statsParamVals2, multiDataS2, multi_sigS2)
  multi_findSigS3 <- findSignificantServer("multi3", multi_statsParamVals3, multiDataS3, multi_sigS3)
  multi_inputFileS1 <- inputFileServer("multi_file1", multi_dataPathS1, multi_errVals1)
  multi_inputFileS2 <- inputFileServer("multi_file2", multi_dataPathS2, multi_errVals2)
  multi_inputFileS3 <- inputFileServer("multi_file3", multi_dataPathS3, multi_errVals3)
  multi_FileName1 <- reactiveVal(value = "File1")
  multi_FileName2 <- reactiveVal(value = "File2")
  multi_FileName3 <- reactiveVal(value = "File3")
  multi_FileNameS1<- multiFileNameServer("multi_file1", "File1", multi_FileName1)
  multi_FileNameS2<- multiFileNameServer("multi_file2", "File2", multi_FileName2)
  multi_FileNameS3<- multiFileNameServer("multi_file3", "File3", multi_FileName3)
  
  # track the number of valid datapath
  col1_filled <- reactiveVal(value = 0)
  col2_filled <- reactiveVal(value = 0)
  col3_filled <- reactiveVal(value = 0)
  multi_col_width <- reactiveVal(value = 12)
  observeEvent({col1_filled(); col2_filled(); col3_filled()}, {
    n_col_filled <- col1_filled() + col2_filled() + col3_filled()
    if (n_col_filled > 0) {multi_col_width(12 / n_col_filled)}
  })

  # TODO handle input file error
  # show hide multi file columns based on input
  observeEvent(multi_dataPathS1(), {
    if (!is.null(multi_dataPathS1()) &
        (is.null(multi_errVals1$input_errors) |
        !grepl('Error', multi_errVals1$input_errors, fixed = T))) {
      shinyjs::show(id='multifile_column_1')
      col1_filled(1)
    } else {
      shinyjs::hide(id='multifile_column_1')
      col1_filled(0)
    }
  })
  observeEvent(multi_dataPathS2(), {
    if (!is.null(multi_dataPathS2()) &
        (is.null(multi_errVals2$input_errors) |
        !grepl('Error', multi_errVals2$input_errors, fixed = T))) {
      shinyjs::show(id='multifile_column_2')
      col2_filled(1)
    } else {
      shinyjs::hide(id='multifile_column_2')
      col2_filled(0)
    }
  })
  observeEvent(multi_dataPathS3(), {
    if (!is.null(multi_dataPathS3()) &
        (is.null(multi_errVals3$input_errors) |
        !grepl('Error', multi_errVals3$input_errors, fixed = T))) {
      shinyjs::show(id='multifile_column_3')
      col3_filled(1)
    } else {
      shinyjs::hide(id='multifile_column_3')
      col3_filled(0)
    }
  })
  
  # wrap the multifile column in one uiOutput for dynamic hide/show
  observeEvent(multi_col_width(), {
    output$multifile_column_1 <- renderUI({
      column(
        multi_col_width(),
        multi_statsParamsOptions("multi_stats_input1"),
        summaryBox("summary1"),
        drawVolcanoPlot("volcano_plot1"),
        basicPlotInputBox("basic_plot_inputs1"),
        plotGGpairFrame("plot_ggpair1"),
      )
    })
    output$multifile_column_2 <- renderUI({
      column(
        multi_col_width(),
        multi_statsParamsOptions("multi_stats_input2"),
        summaryBox("summary2"),
        drawVolcanoPlot("volcano_plot2"),
        basicPlotInputBox("basic_plot_inputs2"),
        plotGGpairFrame("plot_ggpair2"),
      )
    })
    output$multifile_column_3 <- renderUI({
      column(
        multi_col_width(),
        multi_statsParamsOptions("multi_stats_input3"),
        summaryBox("summary3"),
        drawVolcanoPlot("volcano_plot3"),
        basicPlotInputBox("basic_plot_inputs3"),
        plotGGpairFrame("plot_ggpair3"),
      )
    })
  })
  
  
  multiFileLogFCCorrelationDf <- reactiveVal(value = NULL)
  multiFileLogFCCorrelationPlot <- reactiveVal(value = NULL)
  logfcCorrelationBothSigColor <- reactiveVal(value = 'green')
  logfcCorrelationFirstSigColor <- reactiveVal(value = '#FF00D7')
  logfcCorrelationSecondSigColor <- reactiveVal(value = 'orange')
  logfcCorrelationNoSigColor <- reactiveVal(value = '#7F7F80')
  ComparisonPlotParamServer(
    'logfc_comparison',
    logfcCorrelationBothSigColor,
    logfcCorrelationFirstSigColor,
    logfcCorrelationSecondSigColor,
    logfcCorrelationNoSigColor)
  mulitFilelogFCCorrelation <- logFCCorrelationServer(
    'logfc_comparison',
    logfcCorrelationBothSigColor,
    logfcCorrelationFirstSigColor,
    logfcCorrelationSecondSigColor,
    logfcCorrelationNoSigColor,
    multi_sigS1,
    multi_sigS2,
    multi_sigS3,
    multi_FileName1,
    multi_FileName2,
    multi_FileName3,
    multiFileLogFCCorrelationPlot,
    multiFileLogFCCorrelationDf)
  vennDfS <- dataServer("multifile_venn")
  vennDiagram <- plotVennServer('multifile_venn', 
                                multi_sigS1, 
                                multi_sigS2, 
                                multi_sigS3,
                                multi_FileName1,
                                multi_FileName2,
                                multi_FileName3,
                                multi_sharedPlotVals, 
                                vennDfS)
  
  multi_exampleBtn <- getMultiExampleServer(
    "multi_file", 
    c(multi_dataPathS1, multi_dataPathS2, multi_dataPathS3), 
    multiFilePanelUpdate)
  
  # use observe event to update tab items:
  # https://stackoverflow.com/questions/51708815/accessing-parent-namespace-inside-a-shiny-module
  observeEvent(multiFilePanelUpdate(),
               {updateTabItems(session, "sidebarmenu", 'widgets')})
  
  # Servers for rendering UI and controlling I/O
  multi_thresholdTextS1 <- thresholdTextServer("summary1", multi_statsParamVals1, multi_thldVals1)
  multi_thresholdTextS2 <- thresholdTextServer("summary2", multi_statsParamVals2, multi_thldVals2)
  multi_thresholdTextS3 <- thresholdTextServer("summary3", multi_statsParamVals3, multi_thldVals3)
  multi_summaryStatsS1 <- summaryStatsServer("summary1", multi_sigS1)
  multi_summaryStatsS2 <- summaryStatsServer("summary2", multi_sigS2)
  multi_summaryStatsS3 <- summaryStatsServer("summary3", multi_sigS3)
  multi_summaryDisplayS1 <- summaryDisplayServer("summary1", multi_summaryStatsS1, multi_statsParamVals1, multi_thldVals1, multiDataS1, multi_colVals1, multi_errVals1)
  multi_summaryDisplayS2 <- summaryDisplayServer("summary2", multi_summaryStatsS2, multi_statsParamVals2, multi_thldVals2, multiDataS2, multi_colVals2, multi_errVals2)
  multi_summaryDisplayS3 <- summaryDisplayServer("summary3", multi_summaryStatsS3, multi_statsParamVals3, multi_thldVals3, multiDataS3, multi_colVals3, multi_errVals3)
  multi_drawVolcanoS1 <- drawVolcanoServer("volcano_plot1", multi_plotVals1, multi_sigS1, multi_sigColorS1, multi_insigColorS1)
  multi_drawVolcanoS2 <- drawVolcanoServer("volcano_plot2", multi_plotVals2, multi_sigS2, multi_sigColorS2, multi_insigColorS2)
  multi_drawVolcanoS3 <- drawVolcanoServer("volcano_plot3", multi_plotVals3, multi_sigS3, multi_sigColorS3, multi_insigColorS3)
  multi_overlayVolcanoS1 <- overlayVolcanoServer("volcano_plot1", multi_plotVals1, multi_baitS1, multi_goiS1, multi_goiAlphaS1, multi_statsParamVals1)
  multi_overlayVolcanoS2 <- overlayVolcanoServer("volcano_plot2", multi_plotVals2, multi_baitS2, multi_goiS2, multi_goiAlphaS2, multi_statsParamVals2)
  multi_overlayVolcanoS3 <- overlayVolcanoServer("volcano_plot3", multi_plotVals3, multi_baitS3, multi_goiS3, multi_goiAlphaS3, multi_statsParamVals3)
  multi_ggpairS1 <- plotGGpairServer("plot_ggpair1", multi_sigS1, multi_colVals1, multi_plotVals1, multi_sigColorS1, multi_insigColorS1)
  multi_ggpairS2 <- plotGGpairServer("plot_ggpair2", multi_sigS2, multi_colVals2, multi_plotVals2, multi_sigColorS2, multi_insigColorS2)
  multi_ggpairS3 <- plotGGpairServer("plot_ggpair3", multi_sigS3, multi_colVals3, multi_plotVals3, multi_sigColorS3, multi_insigColorS3)
  # basic plot param Box
  multi_basicPlotParamS1 <- basicPlotParamServer("basic_plot_inputs1", multi_baitS1, multi_goiS1, multi_goiAlphaS1, multi_sigColorS1, multi_insigColorS1)
  multi_basicPlotParamS2 <- basicPlotParamServer("basic_plot_inputs2", multi_baitS2, multi_goiS2, multi_goiAlphaS2, multi_sigColorS2, multi_insigColorS2)
  multi_basicPlotParamS3 <- basicPlotParamServer("basic_plot_inputs3", multi_baitS3, multi_goiS3, multi_goiAlphaS3, multi_sigColorS3, multi_insigColorS3)
  
  
  #---------------- TEMPORARY PLACEHOLDER ----------------
  
  # venn diagram inweb mapping for inweb
  a_inweb_calc_hyper <- reactive({
    req(ppiParams()$bait_search)
    req(sigS())
    req(a_ppi_mapping())
    req(a_ppi_mapping_name())
    
    #inweb_output = get_inweb_list(input$a_bait_rep)
    mapping_output = a_ppi_mapping()
    dbname = a_ppi_mapping_name()
    
    if (!is.null(mapping_output)){
      # gather all ppi data
      ppi_list = data.frame(listName=dbname, mapping_output)
      ppi_intersect = data.frame(listName=dbname, intersectN=T)
      data = sigS()
      
      # compile venn diagram information
      hyper = calc_hyper(data, ppi_list, ppi_intersect, bait = baitS())
      hyper[['venn']][['A']] <- hyper$genes[[dbname]]$success_genes # pulldown
      hyper[['venn' ]][['B']] <- hyper$genes[[dbname]]$sample_genes # ppi
      return(hyper)
    } else {NULL}
  })
  
  a_ppi_mapping_df <- reactive({
    req(
      # input$a_bait_rep,
      ppiParams()$bait_search,
      dataS(),
      a_ppi_mapping())
    mapping = a_ppi_mapping()
    #mapping = get_inweb_list(input$a_bait_rep, type = input$a_inweb_type)
    if (!is.null(mapping)){
      mapping = mapping[mapping$significant, ]
      mapping$col_significant = input$a_color_inweb_sig
      mapping$col_other = input$a_color_inweb_insig
      mapping$shape = symbol_to_shape(input$a_symbol_inweb)
      mapping$symbol = input$a_symbol_inweb
      mapping$label = ppiParams()$inweb_label
      mapping$dataset = a_ppi_mapping_name()
      return(mapping)
    } 
  })
  #----------------------------------------------------------
  # Server for storing reactive error messages
  errVals <- errorValues("error_messsages")
  
  # initialize reactive values for plot options
  statsParamVals <- statsParamsValues("stats_input")
  sigColorS <- sigColorServer("sig_color")
  insigColorS <- insigColorServer("insig_color")
  
  
  # Servers for storing genes
  baitS <- baitServer("gene_overlay")
  goiS <- goiServer("gene_overlay")
  goiAlphaS <- goiAlphaServer("gene_overlay")
  
  # Servers for storing plots
  # volcanoPlotS <- volcanoPlotServer("volcano_plot")
  plotVals <- plotValues("volcano_plot")
  # overlaidVolcanoPlotS <- overlaidVolcanoPlotServer("volcano_plot")
  
  # Servers for toggling different panels  
  singleFilePanelUpdate <- triggerPanelUpdateServer("single_file")
  
  # Servers for storing data frame and its associated attributes
  dataS <- dataServer("data")
  sigS <- sigificanceServer("data")
  dataPathS <- dataPathServer("data")
  colVals <- columnsValues("data")
  dataFrameS <- dataFrameServer("data", dataPathS)
  inputErrS <- inputErrorServer("data_check", dataFrameS, errVals)
  accMapErrS <- accessionMapErrorServer(
    "mapping_check", mapAccessionToGeneS, errVals)
  mapAccessionToGeneS <- mapAccessionToGeneServer("data", dataFrameS, errVals)
  statsParamsS <- statsParamsServer(
    "stats_input", mapAccessionToGeneS, dataS, colVals, statsParamVals)
  thldVals <- thresholdsValues("single_file_thresholds")
  enrichmentStatsS <- enrichmentStatsServer(
    "data", mapAccessionToGeneS, statsParamVals, dataS, errVals)
  findSigS <- findSignificantServer("data", statsParamVals, dataS, sigS)
  inputFileS <- inputFileServer("single_file", dataPathS, errVals)
  exampleBtn <- getExampleServer("single_file", dataPathS, singleFilePanelUpdate)
  
  # Servers for rendering UI and controlling I/O
  thresholdTextS <- thresholdTextServer(
    "summary", statsParamVals, thldVals)
  summaryStatsS <- summaryStatsServer(
    "summary", sigS)
  summaryDisplayS <- summaryDisplayServer(
    "summary", summaryStatsS, statsParamVals, thldVals, dataS, colVals, errVals)
  drawVolcanoS <- drawVolcanoServer(
    "volcano_plot", plotVals, sigS, sigColorS, insigColorS)
  overlayVolcanoS <- overlayVolcanoServer(
    "volcano_plot", plotVals, baitS, goiS, goiAlphaS, statsParamVals)
  ggpairS <- plotGGpairServer(
    "plot_ggpair", sigS, colVals, plotVals, sigColorS, insigColorS)
  
  # use observe event to update tab items:
  # https://stackoverflow.com/questions/51708815/accessing-parent-namespace-inside-a-shiny-module
  observeEvent(singleFilePanelUpdate(),
               {updateTabItems(session, "sidebarmenu", 'dashboard')})
  
  # basic plot Box
  basicPlotParamS <- basicPlotParamServer(
    "basic_plot_inputs", baitS, goiS, goiAlphaS, sigColorS, insigColorS)
  
  # Integrated plots
  ppiParams <- overlayExternalPPIServer(
    "ppi_overlay", dataS,a_ppi_mapping_df)
  
  # sliders for selecting color, symbols and labels of plots
  
  # intgrated plot, snp
  output$a_color_snp_sig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_snp_sig', NULL, value = 'blue', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, snp
  output$a_color_snp_insig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_snp_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  
  # integrated plot, snp
  output$a_symbol_snp_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    selectInput('a_symbol_snp', NULL, choices = allowed_plotly_symbols, selected = 'square')
  })
  
  # integrated plot, snp
  output$a_label_snp_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_label_snp", label = "Toggle labels", value = TRUE)
  })
  
  # integrated plot, snp
  output$a_overlay_snp_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_overlay_snp", label = "Toggle overlay", value = TRUE)
  })
  
  # integrated plot, snp,
  output$a_reset_snp_ui <- renderUI({
    actionButton('a_reset_snp','clear')
  })
  
  # intgrated plot, genes upload
  output$a_color_genes_upload_sig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_genes_upload_sig', NULL, value = '#A52A2A', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, genes upload
  output$a_color_genes_upload_insig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_genes_upload_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  
  # integrated plot, genes uplaod
  output$a_symbol_genes_upload_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    selectInput('a_symbol_genes_upload', NULL, choices = allowed_plotly_symbols, selected = 'square')
  })
  
  # integrated plot, genes upload
  output$a_label_genes_upload_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_label_genes_upload", label = "Toggle labels", value = TRUE)
  })
  
  # integrated plot, genes upload
  output$a_overlay_genes_upload_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_overlay_genes_upload", label = "Toggle overlay", value = TRUE)
  })
  
  # integrated plot, reset
  output$a_reset_genes_upload_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    actionButton('a_reset_genes_upload', 'Reset')
  })
  observeEvent(input$a_reset_genes_upload, {
    reset("a_file_genes_rep")
  })
  
  
  # intgrated plot, inweb
  output$a_color_inweb_sig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_inweb_sig', NULL, value = 'yellow', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, inweb
  output$a_color_inweb_insig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_inweb_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # integrated plot, inweb
  output$a_symbol_inweb_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    selectInput('a_symbol_inweb', NULL, choices = allowed_plotly_symbols, selected = 'circle')
  })
  
  # intgrated plot, gwas catalogue
  output$a_color_gwas_cat_sig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_gwas_cat_sig', NULL, value = 'cyan', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, gwas catalogue
  output$a_color_gwas_cat_insig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_gwas_cat_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  
  # integrated plot, gwas catalogue
  output$a_overlay_gwas_cat_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_overlay_gwas_cat", label = "Toggle overlay", value = TRUE)
  })
  
  # integrated plot, gwas catalogue
  output$a_symbol_gwas_cat_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    selectInput('a_symbol_gwas_cat', NULL, choices = allowed_plotly_symbols, selected = 'diamond')
  })
  # integrated plot, genes upload
  output$a_label_gwas_cat_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_label_gwas_cat", label = "Toggle labels", value = TRUE)
  })
  
  # intgrated plot, gnomad
  output$a_color_gnomad_sig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_gnomad_sig', NULL, value = '#FF00FF', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, gnomad
  output$a_color_gnomad_insig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_gnomad_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, gnomad
  output$a_symbol_gnomad_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    selectInput('a_symbol_gnomad', NULL, choices = allowed_plotly_symbols, selected = 'circle')
  })
  
  # intgrated plot, gnomad
  output$a_label_gnomad_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_label_gnomad", label = "Toggle labels", value = FALSE)
  })
  
  # integrated plot, gnomad
  output$a_overlay_gnomad_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_overlay_gnomad", label = "Toggle overlay", value = FALSE)
  })

  # integrated plot, gnomad 
  output$a_select_gnomad_pli_type_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    radioButtons("a_select_gnomad_pli_type", label = 'Select pLI type', 
                 choiceNames = list('Threshold'),
                 choiceValues = list('threshold'))
  })
  
  # integrated plot, gnomad slider
  output$a_slide_gnomad_pli_threshold_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    #validate(need(input$a_select_gnomad_pli_type == 'threshold', ""))
    sliderInput(inputId = "a_slide_gnomad_pli_threshold", label = 'Subset interactors by pLI threshold', 
                min = 0, max = 1, value = 0.9, step = 0.01)
  })
  
  # intgrated plot, tissue
  output$a_color_tissue_sig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    colourpicker::colourInput('a_color_tissue_sig', NULL, value = '#3AFF00', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, tissue
  output$a_color_tissue_insig_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    colourpicker::colourInput('a_color_tissue_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, tissue
  output$a_symbol_tissue_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    selectInput('a_symbol_tissue', NULL, choices = allowed_plotly_symbols, selected = 'circle')
  })
  
  # intgrated plot, tissue
  output$a_label_tissue_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_label_tissue", label = "Toggle labels", value = FALSE)
  })
  
  # integrated plot, tissue
  output$a_overlay_tissue_ui <- renderUI({
    validate(need(dataPathS()  != '', ""))
    checkboxInput("a_overlay_tissue", label = "Toggle overlay", value = TRUE)
  })
  
  # GWAS catalog traits
  gwas_traits <- reactiveVal(value = unique(as.character(as.vector(gwas_table$DISEASE.TRAIT))))
  
  output$a_gwas_catalogue_ui <- renderUI({
    selectInput('a_gwas_catalogue', 'Search GWAS catalog trait(s):', gwas_traits(), multiple=T, selectize=TRUE, selected = "grey")
  })
  
  output$a_gwas_subset_traits_by_data <- renderUI({
    checkboxInput("a_toggle_gwas_subset", label = "Subset entries with traits in data (WARNING: takes a while)", value = FALSE)
  })
  
  output$a_gwas_subset_traits_by_data_freq <- renderUI({
    checkboxInput("a_toggle_gwas_subset_freq", label = "Order entries by frequency", value = FALSE)
  })
  
  observeEvent(input$a_toggle_gwas_subset,{
    input = input$a_toggle_gwas_subset
    if (input){
      showModal(modalDialog(HTML(paste("subsetting GWAS entries by uploaded data.. This may take a while.", '<div class="loader"></div>')), easyClose = T, footer=NULL))
      gwas_traits(a_gwas_catalogue_traits_in_data())
      removeModal()
    } else {
      gwas_traits(unique(as.character(as.vector(gwas_table$DISEASE.TRAIT))))
    }
  })
  
  
  # select PPI DB
  output$a_ppi_select_ui <- renderUI({
    selectInput('a_ppi_select', 'select PPI database',  c("InWeb_InBioMap" = "inweb", "iRefIndex 17.0" = "irefindex", "BioPlex 3.0" = "bioplex"), multiple=F, selectize=TRUE, selected = "grey")
  })
  
  
  # Select GTEx or HPA
  output$a_gtex_rna_tissue_ui <- renderUI({ 
    shinyjs::hidden(selectInput('a_gtex_rna_tissue', 'Annotate specifically expressed genes in tissue(s)', sort(unique(gtex_rna$tissue)), multiple=T, selectize=TRUE, selected = "grey"))
  })
  
  output$a_gtex_protein_tissue_ui <- renderUI({
    shinyjs::hidden(selectInput('a_gtex_protein_tissue', 'Annotate specifically expressed genes in tissue(s)', sort(unique(gtex_protein$tissue)), multiple=T, selectize=TRUE, selected = "grey"))
  })
 
  output$a_hpa_rna_tissue_ui <- renderUI({
    selectInput('a_hpa_rna_tissue', 'Annotate specifically expressed genes in tissue(s)', sort(unique(hpa_rna$tissue)), multiple=T, selectize=TRUE, selected = "grey")
  })  
 
  output$a_tissue_select_ui <- renderUI({
    selectInput('a_tissue_select', 'Select reference dataset',  c("GTEx - RNA" = "GTEx - RNA", "GTEx - Protein" = "GTEx - Protein","HPA - RNA" = "HPA - RNA"), multiple=F, selectize=TRUE, selected = "grey")
  })
  
  # tissue enrichment (Tissue enrichment tab)
  output$a_tissue_enrichment_slider_ui <- renderUI({
    sliderInput('a_tissue_enrichment_slider', 'Select significance threshold', 0.001, 1, value = 0.05, step = 0.001)
  })
  
  
  output$a_tissue_enrichment_upload_ui <- renderUI({
    fileInput('a_tissue_enrichment_upload', 'Upload dataset')                                                                  
  })
  
  
  output$a_tissue_select_source_ui <- renderUI({
    selectInput('a_tissue_select_source', 'Source data',  c("Use pre-existing dataset" = "genoppi", "Upload data" = "upload"), multiple=F, selectize=TRUE, selected = "grey")
  })
  
  output$a_tissue_enrichment_type_select_ui <- renderUI({
    selectInput('a_tissue_enrichment_type_select', 'Select reference dataset', c("GTEx - RNA" = "GTEx - RNA", "GTEx - Protein" = "GTEx - Protein", "HPA - RNA" = "HPA - RNA"), multiple=F, selectize=TRUE, selected = "grey")
  })
  
  output$a_tissue_enrichment_xaxis_ui <- renderUI({
    radioButtons('a_tissue_enrichment_xaxis', 'Format x-axis',c("-log10(P-value)" = 'log10pvalue',
                                                                "-log10(Q-value)" = 'log10qvalue'))
  })
  
  output$a_tissue_enrichment_scientific_notation_ui <- renderUI({
    checkboxInput('a_tissue_enrichment_scientific_notation', 'Scientific notations', value = TRUE)
  })
  
  #
  output$a_select_venn_genes_upload_ui <- renderUI({
    selectInput('a_select_venn_list_genes_upload',
                'Select GENES UPLOAD list', 
                choices = c(a_available_lists_genes_upload(), 'combined'))
                #multiple=T, selectize=TRUE, selected = "grey")
  })

  output$a_select_venn_snp_ui <- renderUI({
    selectInput('a_select_venn_list_snp',
                'Select SNP list', 
                choices = c(a_available_lists_snp(), 'combined'))
                #multiple=T, selectize=TRUE, selected = "grey")
  })
  
  output$a_select_venn_snp_loci_ui <- renderUI({
    req(a_snp_mapping())
    selectInput('a_select_venn_list_snp_loci',
                'Select loci type', 
                choices = c('All loci' = 'all', 
                            'Multi-gene loci' = 'multi', 
                            'Single-gene loci' = 'single'))
  })
  
  
  output$a_SNP_file <- renderUI({
    fileInput('a_file_SNP_rep', 'Tab-delimited file containing two columns: “listName” (name for each list) and “SNP” (rsID):',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv')
    )
  })
  
  output$a_genes_file <- renderUI({
    fileInput('a_file_genes_rep', 'Tab-delimited file containing two columns: “listName” (name for each list) and “gene” (HGNC symbol):',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'), multiple = T
    )
  })
  
  output$a_genes_file_vennd <- renderUI({
    fileInput('a_file_genes_vennd', 'File containing at least 2 genes with header, one HGNC symbol per line (e.g. TINMAN)',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv')
    )
  })
  
  output$a_pf_loc_selection <- renderUI({
    #req(a_file_pulldown_r() ())
    selectInput('a_pf_loc_option', 'Gene sets', c("HGNC gene groups"="hgnc", 
                                                     "GO terms: molecular function"="mf", 
                                                     "GO terms: cellular component"="cc", 
                                                     "GO terms: biological process"="bp", 
                                                     "MSigDB H: hallmark gene sets"="h",
                                                     "MsigDB C1: positional gene sets" = 'c1',
                                                     "MSigDB C2: curated gene sets" = 'c2',
                                                     "MSigDB C3: regulatory target gene sets" = 'c3',
                                                     "MSigDB C4: computational gene sets" = 'c4',
                                                     "MSigDB C5: GO terms" = 'c5',
                                                     "MSigDB C6: oncogenic signatures" = 'c6',
                                                     "MSigDB C7: immunologic signatures" = 'c7'), selectize=FALSE)
  })
  
  output$a_pathway_mapping_freq_slider_ui <- renderUI({
    req(sigS())
    freq = a_pathway_mapping_values()$Freq
    fmax = ifelse(is.null(freq), 1, max(freq))
    fmin = ifelse(is.null(freq), 1, a_pathway_mapping_freq_lowest_allowed()) #ifelse(is.null(freq), 1, min(freq)) 
    sliderInput("a_pathway_mapping_freq_slider", "Subset by frequency",
                min = fmin, max = fmax, value = c(fmin, fmax), step = 1)
  })
  
  output$a_pathway_mapping_type_sort_ui <- renderUI({
    req(sigS())
    radioButtons('a_pathway_mapping_type_sort', 'Sort legend',
                 c('alphabetically' = 'alpha',
                   'frequency' = 'freq'), 
                 inline = T)
    
  })
  
  
  output$a_pathway_mapping_search_ui <- renderUI({
    req(sigS())
    mapping = a_pathway_mapping_values()
    mapping = mapping[rev(order(mapping$Freq)), ]
    selectInput('a_pathway_mapping_search', 'Search gene set', unique(mapping$pathway), multiple=T, selectize=TRUE, selected = "grey")
  })

  # function for handling uploaded genes
  a_genes_upload <- reactive({
    filepath = input$a_file_genes_rep$datapath
    if (!is.null(filepath)){
      genes = get_gene_lists(filepath)
      genes$data$dataset = 'Genes upload'
      genes$data$col_significant = input$a_color_genes_upload_sig
      genes$data$col_other = input$a_color_genes_upload_insig
      genes$data$shape = symbol_to_shape(input$a_symbol_genes_upload)
      genes$data$symbol = input$a_symbol_genes_upload
      genes$data$label = input$a_label_genes_upload
      #if (!is.null(genes$data$listName)) genes$data$alt_label <- genes$data$listName
      if (lun(genes$data$listName) > 1) genes$data$alt_label <- genes$data$listName
      return(genes)
    }
  })
  
  #snp to gene using LD r^2>0.6±user defined extension
  a_snp <- reactive({
    req(input$a_file_SNP_rep)
    dsnp = data.table::fread(input$a_file_SNP_rep$datapath, header=T)
    return(dsnp)
  })
  
  # read in the snps from a file
  a_snp_mapping <- reactive({
    mapping = get_snp_lists(infile = a_snp(), dataS()$gene)
    mapping$col_significant = input$a_color_snp_sig
    mapping$col_other = input$a_color_snp_insig
    mapping$shape = symbol_to_shape(input$a_symbol_snp)
    mapping$symbol = input$a_symbol_snp
    mapping$label = input$a_label_snp
    mapping$dataset = 'SNP upload'
    mapping$alt_label = mapping$SNP
    if (lun(mapping$listName) > 1) mapping$alt_label <- paste0(mapping$alt_label, ' (',mapping$listName,')')
    return(mapping)
  })
  
  # take selected bait and find preys in selected PPI database
  a_ppi_mapping <- reactive({
    req(ppiParams()$bait_search, 
        dataS(),
        ppiParams()$selected_ppi)
    mapping = NULL
    db = ppiParams()$selected_ppi
    if(db == 'inweb' & !is.null(ppiParams()$inweb_type)){
      mapping = get_inweb_list(
        ppiParams()$bait_search, 
        type = ppiParams()$inweb_type)
    }
    if(db == 'bioplex' & !is.null(ppiParams()$bioplex_prob)) {
      mapping = get_bioplex_list(
        ppiParams()$bait_search,
        p = ppiParams()$bioplex_prob)
    }
    if(db == 'irefindex' & !is.null(ppiParams()$irefindex_min_pub)) {
      mapping = get_irefindex_list(
        ppiParams()$bait_search, 
        n = ppiParams()$irefindex_min_pub)
    }
    return(mapping)
  })
  
  # keep track of name of PPI database, and add lower/upper case
  a_ppi_mapping_name <- reactive({
    req(a_ppi_mapping())
    name = NULL
    # db = input$a_ppi_select
    db = ppiParams()$selected_ppi
    if (db == 'inweb') name = 'Inweb'
    if (db == 'bioplex') name = 'Bioplex'
    if (db == 'irefindex') name = 'IRefIndex'
    return(name)
  })
  
  # map inweb proteins
  # a_ppi_mapping_df <- reactive({
  #   req(
  #     # input$a_bait_rep,
  #     ppiParams()$bait_search,
  #     # a_pulldown(), 
  #     dataS(),
  #     a_ppi_mapping())
  #   mapping = a_ppi_mapping()
  #   #mapping = get_inweb_list(input$a_bait_rep, type = input$a_inweb_type)
  #   if (!is.null(mapping)){
  #       mapping = mapping[mapping$significant, ]
  #       mapping$col_significant = input$a_color_inweb_sig
  #       mapping$col_other = input$a_color_inweb_insig
  #       mapping$shape = symbol_to_shape(input$a_symbol_inweb)
  #       mapping$symbol = input$a_symbol_inweb
  #       mapping$label = ppiParams()$inweb_label
  #       mapping$dataset = a_ppi_mapping_name()
  #       return(mapping)
  #   } 
  # })
  
  # map gwas catalouge
  a_gwas_catalogue_mapping <- reactive({
    req(input$a_gwas_catalogue, dataS())
    genes = dataS()$gene
    validate(need(!all(is.na(genes)), ""))
    mapping = get_gwas_lists(input$a_gwas_catalogue, genes)
    if (!is.null(mapping)){
      mapping$col_significant = input$a_color_gwas_cat_sig
      mapping$col_other = input$a_color_gwas_cat_insig
      mapping$shape = symbol_to_shape(input$a_symbol_gwas_cat)
      mapping$symbol = input$a_symbol_gwas_cat
      mapping$label = input$a_label_gwas_cat
      #mapping$alt_label = mapping$SNP
      mapping$alt_label = paste(mapping$DISEASE.TRAIT,'-',mapping$SNP, paste0('(PMID:',mapping$PUBMEDID,' study P-value:',mapping$P.VALUE,')'))
      mapping$dataset = 'GWAS catalog'
      return(mapping)
    } 
  })
  
  # traits in data
  a_gwas_catalogue_traits_in_data <- eventReactive(input$a_toggle_gwas_subset,{
    genes = as.character(dataS()$gene)
    
    # map genes to snps and find gwas table entry
    tabl = lapply(genes, function(x) gwas_table$SNP %in% genes_snps[[x]])
    tabl_bool = apply(do.call(cbind, tabl), 1, any)
    tabl_subset = gwas_table[tabl_bool, ]
    result = tabl_subset$DISEASE.TRAIT

    return(unique(result))
  })
  
  # setup main gnomad mapping
  a_gnomad_mapping <- reactive({
    req(dataS(), input$a_slide_gnomad_pli_threshold)
    pulldown = dataS()
    validate(need(!all(is.na(pulldown$gene)), ""))
    gnomad = merge(pulldown, gnomad_table, by = 'gene')
    gnomad$dataset = 'gnomAD'
    gnomad$alt_label = paste0('pLI=',gnomad$pLI)
    return(gnomad)
  })
  
  # do cutoff colorscale
  a_gnomad_mapping_threshold <- reactive({
    req(a_gnomad_mapping(), input$a_slide_gnomad_pli_threshold)
    gnomad = a_gnomad_mapping()
    bool_threshold = gnomad$pLI >= input$a_slide_gnomad_pli_threshold
    bool_threshold[is.na(bool_threshold)] <- FALSE
    gnomad = gnomad[bool_threshold,]
    gnomad$col_significant = input$a_color_gnomad_sig 
    gnomad$col_other = input$a_color_gnomad_insig 
    gnomad$shape = symbol_to_shape(input$a_symbol_gnomad)
    gnomad$symbol = input$a_symbol_gnomad
    gnomad$label = input$a_label_gnomad
    return(gnomad)
  })
  
  # for calculating hypergeometric p-value
  # ideally, this should be a function in the future
  a_gnomad_sig_list <- reactive({
    req(sigS(), input$a_slide_gnomad_pli_threshold)
    pulldown = sigS()
    threshold = gnomad_table$gene %in% pulldown$gene & gnomad_table$pLI >= input$a_slide_gnomad_pli_threshold
    threshold[is.na(threshold)] = FALSE
    return(data.frame(gene=gnomad_table$gene, significant=threshold))
  })
  
  ## upload own tissue enrichment in 'long' format (tissue, gene, sig)
  ## upload own tissue enrichment in matrix format (tissue x gene with sig in each cell)
  
  # upload own tissue list
  a_get_tissue_upload <- reactive({
    req(input$a_tissue_enrichment_upload, input$a_tissue_select_source)
    validate(need(input$a_tissue_select_source == 'upload'  , ""))
    file = input$a_tissue_enrichment_upload
    table = read.table(file$datapath, header = T, sep="\t")
    if (ncol(table) == 3){
      return(table)
    } else {
      return(NULL)
    }
  })
  
  
  # return HPA or GTEx query
  a_get_tissue_list <- reactive({
    selected = input$a_tissue_select
    req(sigS(), selected)
    if (selected %in% 'HPA - RNA' & length(input$a_hpa_rna_tissue) > 0){
      return(get_tissue_lists(tissue = as.character(input$a_hpa_rna_tissue), table = hpa_rna))
    
    } else if (selected %in% 'GTEx - RNA' & length(input$a_gtex_rna_tissue) > 0){
      return(get_tissue_lists(tissue = as.character(input$a_gtex_rna_tissue), table = gtex_rna))
    
    } else if (selected %in% 'GTEx - Protein' & length(input$a_gtex_protein_tissue) > 0){
      return(get_tissue_lists(tissue = as.character(input$a_gtex_protein_tissue), table = gtex_protein))
   
    } else {
      return(NULL)
    }
  })
  
  # setup main mapping
  a_tissue_mapping <- reactive({
    req(sigS(), a_get_tissue_list())
    pulldown = sigS()
    validate(need(!all(is.na(pulldown$gene)), ""))
    tissue = a_get_tissue_list() 
    tissue = tissue[tissue$significant, ]
    tissue = merge(pulldown, tissue, by = 'gene')
    if (nrow(tissue)){
      tissue$dataset = input$a_tissue_select
      tissue$col_significant = input$a_color_tissue_sig 
      tissue$col_other = input$a_color_tissue_insig 
      tissue$shape = symbol_to_shape(input$a_symbol_tissue)
      tissue$symbol = input$a_symbol_tissue
      tissue$label = input$a_label_tissue
      tissue$alt_label = paste0('Tissue elevated gene in ', tissue$tissue,'.')
      return(tissue)
    } else {
      return(NULL)
    }
  })
  


  # get the tissue enrichment table that have been selected by user
  a_tissue_enrichment_table <- reactive({
    req(sigS(), input$a_tissue_enrichment_type_select)
    tissue = input$a_tissue_enrichment_type_select
    source = input$a_tissue_select_source
    if (source == 'genoppi'){
      if (input$a_tissue_enrichment_type_select == 'HPA - RNA') {
        return(hpa_rna)
      } else if (input$a_tissue_enrichment_type_select == 'GTEx - RNA') {
        return(gtex_rna)
      } else {
        return(gtex_protein)
      }
    } else {
      return(a_get_tissue_upload())
    }

  })
  
  # Only render when button has been pressed.
  output$a_button_plot_tissue_enrichment_ui <- renderUI({
    actionButton('a_button_plot_tissue_enrichment', 'Render plot')
  })
  
  # get tissue with elevated expression and calculate FDR.
  a_tissue_enrichment <- eventReactive(input$a_button_plot_tissue_enrichment,{
    req(sigS(), a_tissue_enrichment_table())
    pulldown =sigS()
    table = a_tissue_enrichment_table()
    enrichment = lapply_calc_hyper(pulldown, table)
    enrichment$log10pvalue <- -log10(enrichment$pvalue)
    enrichment$log10qvalue <- -log10(enrichment$BH.FDR) 
    enrichment$bhfdr <- enrichment$BH.FDR
    return(enrichment)
  })
  
  # controls what should be returned to enrichment plot
  a_tissue_enrichment_layout <- eventReactive(input$a_button_plot_tissue_enrichment,{
    req(sigS(), input$a_tissue_enrichment_xaxis, input$a_tissue_enrichment_slider)
    
    # setup switches
    make_xlab <- function(type) {
      switch(type,
             log10pvalue = '-log10(<i>P</i>-value)',
             log10qvalue = '-log10(<i>Q</i>-value)')
    }
    
    # save layout to list
    layout <- list()
    layout$xaxis = input$a_tissue_enrichment_xaxis
    layout$sig_line = -log10(input$a_tissue_enrichment_slider)
    layout$xlab = make_xlab(input$a_tissue_enrichment_xaxis)
    layout$title = ifelse(input$a_tissue_enrichment_type_select == 'HPA - RNA',
                          'Proteomic data enriched in Human Protein Atlas',
                          'Proteomic data enriched in GTEx')

    return(layout)
  })
  
  
  # generate the hovertext for tissue enrichment bar plot
  tissue_enrichment_hovertext <- reactive({
    req(a_tissue_enrichment, !is.null(input$a_tissue_enrichment_scientific_notation))
    df = a_tissue_enrichment()
    scientific = input$a_tissue_enrichment_scientific_notation
    
    # collect data to be displayed
    pvalues = ifelse(scientific, list(formatC(df$pvalue, format = "e", digits = 2)), list(df$pvalue))
    fdr = ifelse(scientific, list(formatC(df$bhfdr, format = "e", digits = 2)), list(df$bhfdr))
    genes = unlist(lapply(df$successInSampleGenes, function(x) paste(strwrap(x, width = 40), collapse = '<br>')))
    return(paste0('P-value: ', unlist(pvalues), '. ', 'Q-value (FDR): ', unlist(fdr), '. <br>',
                      bold(df$successInSample_count), ' genes(s) enriched in tissue: <br>',
                      italics(genes), '.'))
  })
  
  # plot bar chart of tissue enrichment
  output$a_tissue_enrichment_ui <- plotly::renderPlotly({
    req(a_tissue_enrichment(), a_tissue_enrichment_layout())
    
    # get enrichment data and format visuals
    df = a_tissue_enrichment()
    layout = a_tissue_enrichment_layout()
    hovertext = tissue_enrichment_hovertext()
    df$significant = as.factor(ifelse(df[[layout$xaxis]] > layout$sig_line, 'significant', 'not-significant'))
    df$details = hovertext
    
    # plot data
    plotly_tissue_enrichment(df, 
                             'list_name', 
                             layout$xaxis,
                             'details',
                             pvalue.line = layout$sig_line, 
                             xlab = HTML(layout$xlab), 
                             ylab = 'Tissue')
    
  })
  
  
  #---------------------------------------------------------------
  # download different mappings and hide/show download buttons
  
  # download gene upload mapping
  output$a_gene_upload_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-gene-upload-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = sigS()
      upload = a_genes_upload()$data[,c('gene','listName')]
      mymerge = merge(pulldown, upload, by = 'gene')
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  # download snp mapping
  output$a_snp_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-snps-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = sigS()
      snp = a_snp_mapping()[,c('dataset','gene', 'SNP')]
      mymerge = merge(pulldown, snp, by = 'gene')
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  # gwas catalogue mapping download
  output$a_gwas_catalogue_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-gwas-catalog-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = sigS()
      gwas = a_gwas_catalogue_mapping()[c("gene", "SNP","P.VALUE", "DISEASE.TRAIT", "PUBMEDID", "STUDY.ACCESSION")]
      mymerge = merge(pulldown, gwas, by = 'gene')
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  # download gnomAD mapping
  output$a_gnomad_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-gnomad-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = sigS()
      gnomad = a_gnomad_mapping_threshold()[,c('gene','logFC','pvalue','FDR','significant','pLI')]
      mymerge = merge(pulldown, gnomad, by = 'gene')
      write.csv(mymerge, file, row.names = F)
    }
   )
  
  # download tissue mapping
  output$a_tissue_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-tissue-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = sigS()
      hpa = a_tissue_mapping()[,1:15]
      write.csv(hpa, file, row.names = F)
    }
  )
  
  # download tissue enrichment data
  output$a_tissue_enrichment_download <- downloadHandler(
    filename = function() {
      paste("genoppi-tissue-enrichment",".csv", sep="")
    },
    content = function(file) {
      result = a_tissue_enrichment()
      result$bhfdr <- NULL
      result$log10pvalue <- NULL
      write.csv(result, file, row.names = F)
    }
  )
  

  # download protein family mapping? gene annotations
  output$a_pathway_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-geneset-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = sigS()
      pathway = a_pathway_mapping()[,c('gene','pathway','Freq')]
      mymerge = merge(pulldown, pathway, by = 'gene')
      mymerge$database = input$a_pf_loc_option
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  ### venn diagrams ###
  # venn diagram mapping for gnomad
  output$a_gnomad_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-gnomad-venn-mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_gnomad_calc_hyper()$venn
      write.csv(venn_to_table(venn), file, row.names = F)
    }
  )
  
  # venn diagram mapping for gwas catalogue
  output$a_gwas_catalogue_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-gwas-catalog-venn-mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_gwas_catalogue_mapping_venn()
      diagram = venn_to_table(venn)
      mapping = a_gwas_catalogue_mapping()[,c('gene','SNP', 'P.VALUE','PUBMEDID', 'STUDY.ACCESSION', 'DISEASE.TRAIT')]
      mymerge = merge(diagram, mapping, by = 'gene', all.x = T)
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  # venn diagram human protein atlas
  output$a_tissue_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-tissue-venn-mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_tissue_calc_hyper()$venn
      write.csv(venn_to_table(venn), file, row.names = F)
    }
  )
  
  # venn diagram mapping snps
  output$a_snp_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-snp-venn-mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_snp_venn()
      diagram = venn_to_table(venn)
      mapping = a_snp_mapping()[,c('gene','listName','SNP')]
      mymerge = merge(diagram, mapping, by = 'gene', all.x = T)
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  # venn diagram mapping for uploaded genes
  output$a_genes_upload_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-genes-upload-venn-mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_genes_upload_venn()
      diagram = venn_to_table(venn)
      mapping = a_genes_upload()$data[,c('gene','listName')]
      mymerge = merge(diagram, mapping, by = 'gene', all.x = T)
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  # show/hide data download buttons
  # observeEvent(a_file_pulldown_r() , {shinyjs::toggle(id="a_mttest_mapping_download", condition=!is.null(a_file_pulldown_r() ))})
  # observeEvent(input$a_bait_rep, {shinyjs::toggle(id="a_ppi_mapping_df_download", condition=!is.null(a_pulldown_significant()) & any(input$a_bait_rep %in% c(inweb_table$Gene1,inweb_table$Gene2)))})
  observe({shinyjs::toggle(id="a_snp_mapping_download", condition=!is.null(sigS()) & !is.null(input$a_file_SNP_rep$datapath))})
  observe({shinyjs::toggle(id="a_gene_upload_mapping_download", condition=!is.null(sigS()) & !is.null(input$a_file_genes_rep))})
  observe({shinyjs::toggle(id="a_gwas_catalogue_mapping_download", condition=!is.null(sigS()) & !is.null(input$a_gwas_catalogue))})
  observe({shinyjs::toggle(id="a_gnomad_mapping_download", condition=!is.null(sigS()) )})
  #observe({shinyjs::toggle(id="a_tissue_mapping_download", condition=!is.null(a_pulldown_significant() & !is.null(a_tissue_mapping())))}) # this seems to cause GCP to crash?
  observe({shinyjs::toggle(id="a_pathway_mapping_download", condition=!is.null(sigS()))})
  observe({shinyjs::toggle(id="a_tissue_enrichment_download", condition=!is.null(sigS()))})  

  # venn diagrams
  # REC
  # observeEvent(input$a_bait_rep, {shinyjs::toggle(id="a_inweb_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & any(input$a_bait_rep %in% c(inweb_table$Gene1,inweb_table$Gene2)))})
  observe({shinyjs::toggle(id="a_snp_venn_mapping_download", condition=!is.null(sigS()) & !is.null(input$a_file_SNP_rep$datapath))})
  observe({shinyjs::toggle(id="a_genes_upload_venn_mapping_download", condition=!is.null(sigS()) & !is.null(input$a_file_genes_rep))})
  observe({shinyjs::toggle(id="a_gwas_catalogue_venn_mapping_download", condition=!is.null(sigS()) & !is.null(input$a_gwas_catalogue))})
  observe({shinyjs::toggle(id="a_gnomad_venn_mapping_download", condition=!is.null(sigS()) & !is.null(a_gnomad_mapping_threshold()) )})
  observe({shinyjs::toggle(id="a_tissue_venn_mapping_download", condition=!is.null(sigS()) & !is.null(input$a_tissue_select) & !is.null(input$a_gtex_rna_tissue) )})
  
  # show hide select buttons (HPA/GTEx)
  observe({shinyjs::toggle(id="a_hpa_rna_tissue", condition = input$a_tissue_select == 'HPA - RNA')})
  observe({shinyjs::toggle(id="a_gtex_rna_tissue", condition = input$a_tissue_select == 'GTEx - RNA')})
  observe({shinyjs::toggle(id="a_gtex_protein_tissue", condition = input$a_tissue_select == 'GTEx - Protein')})
  observe({shinyjs::toggle(id="a_tissue_enrichment_type_select", condition=!is.null(sigS()) & input$a_tissue_select_source == 'genoppi')})
  observe({shinyjs::toggle(id="a_tissue_enrichment_upload", condition=!is.null(sigS()) & input$a_tissue_select_source == 'upload')})
  
  # show hide select PPI DBs
  observe({shinyjs::toggle(id="a_inweb_type", condition = input$a_ppi_select == 'inweb')})
  observe({shinyjs::toggle(id="a_bioplex_type", condition = input$a_ppi_select == 'bioplex')})
  observe({shinyjs::toggle(id="a_irefindex_type", condition = input$a_ppi_select == 'irefindex')})  
  
  # show hide alpha sliders
  # observe({shinyjs::toggle(id="a_goi_search_rep_alpha", condition = input$a_goi_search_rep != '')})
  observe({shinyjs::toggle(id="b_goi_search_rep_alpha", condition = input$b_goi_search_rep != '')})
  
  # show/hide plot download buttons
  observeEvent(!is.null(sigS()),{
    #shinyjs::show("a_tissue_select_source")
    #shinyjs::show("a_tissue_enrichment_type_select")
    # shinyjs::show("a_volcano_plot_download")
    shinyjs::show("a_scatter_plot_download")
    shinyjs::show("a_integrated_plot_download")
    shinyjs::show("a_pathway_plot_download")
    shinyjs::show("a_pathway_plot_legend_download")
  })
  
  #--------------------------------------------------------
  # venn digrams and hypergeometric testing
  
  # INWEB
  # inweb hypergeometric overlap
  # a_inweb_calc_hyper <- reactive({
  #   req(ppiParams()$bait_search)
  #   req(dataS())
  #   req(a_ppi_mapping())
  #   req(a_ppi_mapping_name())
  #   # req(
  #   #   # input$a_bait_rep, 
  #   #   ppiParams()$bait_search,
  #   #   # a_pulldown_significant(),
  #   #   dataS(),
  #   #   a_ppi_mapping(), 
  #   #   a_ppi_mapping_name())
  # 
  #   #inweb_output = get_inweb_list(input$a_bait_rep)
  #   mapping_output = a_ppi_mapping()
  #   dbname = a_ppi_mapping_name()
  # 
  #   if (!is.null(mapping_output)){
  #     # gather all ppi data
  #     ppi_list = data.frame(listName=dbname, mapping_output)
  #     ppi_intersect = data.frame(listName=dbname, intersectN=T)
  #     # data = a_pulldown_significant()
  #     data = dataS()
  # 
  #     # compile venn diagram information
  #     hyper = calc_hyper(data, ppi_list, ppi_intersect, bait = baitS())
  #     hyper[['venn']][['A']] <- hyper$genes[[dbname]]$success_genes # pulldown
  #     hyper[['venn' ]][['B']] <- hyper$genes[[dbname]]$sample_genes # ppi
  #     return(hyper)
  #   } else {NULL}
  # })
  
  # draw venn diagram
  output$a_inweb_venn_ui <- renderPlot({
    req(
      # input$a_bait_rep,
      ppiParams()$bait_search,
      sigS(),
      a_inweb_calc_hyper())
    hyper = a_inweb_calc_hyper()
    v = draw_genoppi_venn(hyper$venn, 
                          color = c('blue','yellow'),#c(input$a_color_indv_sig, input$a_color_inweb_sig), 
                          main = paste0('P-value = ', format(hyper$statistics$pvalue, digits = 3)))
    grid::grid.newpage()
    grid::grid.draw(v)
  })
  
  # plot below venn diagram inweb
  a_ppi_venn_verbatim <- reactive({
    req(
      sigS(),
      a_inweb_calc_hyper(), 
      # input$a_bait_rep, 
      ppiParams()$bait_search,
      a_ppi_mapping_name())
    thresholds = paste(thldVals$sigTxt, thldVals$fcSigTxt, sep =', ')
    db = a_ppi_mapping_name()
    hyper = a_inweb_calc_hyper()
    A <- paste0("A = proteomic data subsetted by ", thresholds, " &#40;", bold(hyper$statistics$success_count), "&#41;")
    B <- paste0("B = ", bold(input$a_bait_rep)," ", db," interactors", " &#40;", bold(hyper$statistics$sample_count), "&#41;")
    total <- paste0("Total population = proteomic data &cap; ", db," &#40;", bold(hyper$statistics$population_count), "&#41;")
    return(list(A=A, B=B, total=total))
  })
  
  # message if Inweb can not be found
  output$a_ppi_message <- renderUI({
    req(ppiParams()$bait_search, dataS(), ppiParams()$selected_ppi)
    
    # get info about current selection
    query = ppiParams()$bait_search
    mapping = a_ppi_mapping()
    data = dataS()
    
    # send message to UI
    if (is.null(mapping)){
      return(HTML(paste(query, 'was not found in the database!')))
    } else if (query %nin% data$gene){
      return(HTML(paste(query,'was not found in proteomic data.')))
    }
  })
  
  output$a_inweb_venn_table_ui <- reactive({
    req(
      sigS(),
      a_inweb_calc_hyper())
    hyper = a_inweb_calc_hyper() #$genes$InWeb$successInSample_genes
    x = as.character(hyper$venn$Pulldown)
    y = as.character(hyper$venn$InWeb)
    genes = data.frame(gene = unique(c(x, y)), dataset=NA)
    genes[genes$gene %in% x,]$dataset <- 'Pulldown'
    genes[genes$gene %in% y, ]$dataset <- 'InWeb'
    genes[genes$gene %in% x & genes$gene %in% y, ]$dataset <- 'Overlap'
  })
  
  # print to ui
  output$a_ppi_venn_verbatim_ui <- renderUI({
    output <- a_ppi_venn_verbatim()
    HTML(paste(output$total, output$A, output$B, sep = "<br/>"))
  })
  
  ## GENES UPLOAD
  # hypergeometric overlap gene upload
  a_genes_upload_calc_hyper <- reactive({
    req(a_genes_upload(), sigS(), input$a_select_venn_list_genes_upload)
    
    # get data for overlap calculation
    pulldown = sigS()
    genes_uploaded = a_genes_upload()
    genes = genes_uploaded$data
    intersect = genes_uploaded$intersect
    intersect$intersectN = F # ask yu-han (why does this always return intersectN = TRUE)
    genelist = input$a_select_venn_list_genes_upload
    
    # compile venn diagram information
    intersect_selected = intersect[intersect$listName %in% genelist | genelist == 'combined',]
    genes_selected = genes[genes$listName %in% genelist | genelist == 'combined',]
    hyper = calc_hyper(pulldown, genes_selected, intersect_selected, a_bait_parsed())
    
    return(hyper)
  })
  
  # get a vector of available lists that have been uploaded
  a_available_lists_genes_upload <- reactive({
    req(a_genes_upload())
    return(unique(as.character(a_genes_upload()$data$listName)))
  })
  
  # instructions for creating the venn diagram
  a_genes_upload_venn <- reactive({
    req(a_genes_upload_calc_hyper(), input$a_select_venn_list_genes_upload)
    # get data and variables for genelist
    genelist = input$a_select_venn_list_genes_upload
    mapping = a_genes_upload_calc_hyper()
    if (!is.null(mapping$genes)){
      diagram = list(
        pulldown = mapping$genes[[1]]$success_genes,
        geneslist = mapping$genes[[1]]$sample_genes)
      names(diagram) <- c('A', 'B')
      return(diagram)
    } else {NULL}
  })
  
  # Reactive for drawing the actual venn in the UI
  output$a_genes_upload_venn_ui <- renderPlot({
    req(a_genes_upload_venn(), a_genes_upload_calc_hyper())
    diagram = a_genes_upload_venn()
    mapping = a_genes_upload_calc_hyper()
    v = draw_genoppi_venn(diagram,color = c('blue','cyan'), main = paste0('P-value = ', format(mapping$statistics$pvalue, digits = 3)))
    grid::grid.newpage()
    grid::grid.draw(v)
  })
  
  # Collect all the information next to venn diagram
  a_genes_upload_venn_verbatim <- reactive({
    req(sigS(), a_genes_upload_venn(), input$a_select_venn_list_genes_upload)
    selected = input$a_select_venn_list_genes_upload
    pulldown = sigS()
    thresholds = paste(thldVals$sigTxt, thldVals$fcSigTxt, sep =', ')
    diagram = a_genes_upload_venn()
    A <- paste0("A = proteomic data subsetted by ", thresholds, " &#40;", bold(length(diagram[[1]])), "&#41;")
    B <- paste0("B = Genes in ",italics(selected)," &#40;", bold(length(unique(diagram[[2]]))), "&#41;")
    total <- paste0("Total population = proteomic data &#40;", bold(nrow(pulldown)), "&#41;")
    return(list(A=A, B=B, total=total))
  })
  
  # Send to UI
  output$a_genes_upload_venn_verbatim_ui <- renderUI({
    output <- a_genes_upload_venn_verbatim()
    HTML(paste(output$total, output$A, output$B, sep = "<br/>"))
  })
  
  ## Human Protein Atlas and GTEX
  # calculate hypergeometric overlap
  a_tissue_calc_hyper <- reactive({
    req(input$a_tissue_select, sigS())
    output = a_get_tissue_list() 
    if (!is.null(output)){
      # setup data for calculating hypergeom. P-value.
      listname = toupper(input$a_tissue_select)
      output_list = data.frame(listName = listname, output)
      output_intersect = data.frame(listName = listname, intersectN = T)
      data = sigS()
      # compile venn diagram information
      hyper = calc_hyper(data, output_list, output_intersect, bait = NULL) #a_bait_parsed())
      hyper[['venn']][['A']] <- hyper$genes[[listname]]$success_genes # pulldown
      hyper[['venn' ]][['B']] <- hyper$genes[[listname]]$sample_genes # inweb
      return(hyper)
    } else {NULL}
  })
  
  # draw venn diagram
  output$a_tissue_venn_ui <- renderPlot({
    req(input$a_tissue_select, sigS(), a_tissue_calc_hyper())
    hyper = a_tissue_calc_hyper()
    v = draw_genoppi_venn(hyper$venn, color = c('blue','red'),
                          main = paste0('P-value = ', format(hyper$statistics$pvalue, digits = 3)))
    grid::grid.newpage()
    grid::grid.draw(v)
  })
  
  
  # text to be displayed alongside venn diagram
  a_tissue_venn_verbatim <- reactive({
    req(sigS(), a_tissue_calc_hyper(), input$a_tissue_select)
    
    # get text to be displayed
    thresholds = paste(thldVals$sigTxt, thldVals$fcSigTxt, sep =', ')
    dataset = input$a_tissue_select
    if (input$a_tissue_select == 'HPA - RNA') tissue <- input$a_hpa_rna_tissue
    if (input$a_tissue_select == 'GTEx - Protein') tissue <- input$a_gtex_protein_tissue
    if (input$a_tissue_select == 'GTEx - RNA') tissue <- input$a_gtex_rna_tissue
    
    # get hypergeometric stats
    hyper = a_tissue_calc_hyper()
    A <- paste0("A = proteomic data subsetted by ", thresholds, " &#40;", bold(hyper$statistics$success_count), "&#41;")
    B <- paste0("B = ", bold(paste0(tissue, collapse = '; '))," (", dataset,")", " &#40;", bold(hyper$statistics$sample_count), "&#41;")
                total <- paste0("Total population = proteomic data &cap; ", dataset," &#40;", bold(hyper$statistics$population_count), "&#41;")
                return(list(A=A, B=B, total=total))
  })
    
  # Send text to ui
  output$a_tissue_venn_verbatim_ui <- renderUI({
    output <- a_tissue_venn_verbatim()
    HTML(paste(output$total, output$A, output$B, sep = "<br/>"))
  })
    
  
  # SNPS UPLOAD
  # get available snp lists
  a_available_lists_snp <- reactive({
    return(unique(as.character(a_snp_mapping()$listName)))
  })
  
  # hypergeometric overlap gene upload
  a_snp_draw_venn <- reactive({
    req(sigS(), input$a_select_venn_list_snp, a_snp_mapping())
    snplist = input$a_select_venn_list_snp
    
    # get data for venn
    snps_uploaded = a_snp_mapping()
    snps_selected = snps_uploaded[snps_uploaded$listName %in% snplist | snplist == 'combined',]
    mapping = subset_snp_loci(snps_selected)
    
    # generate venn
    return(list(venn=mapping))
  })
  
  # make venn diagram instructions
  a_snp_venn <- reactive({
    req(a_snp_draw_venn(), sigS(), input$a_select_venn_list_snp_loci)
    
    # variables and data for drawing venn
    loci = paste0(input$a_select_venn_list_snp_loci,'GeneDf')
    snplist = input$a_select_venn_list_snp
    pulldown = sigS()
    mapping = a_snp_draw_venn()
    
    # draw venn digram if mapping is valid
    if (!is.null(mapping)){
      diagram = list(pulldown=pulldown[pulldown$significant,]$gene,
                     genelist=as.character(mapping$venn[[loci]]$gene))
      names(diagram) = c('A','B')
      return(diagram)
    } else {NULL}
  })
  
  # draw venn diagram for genes upload
  output$a_snp_venn_ui <- renderPlot({
    req(a_snp_venn())
    diagram = a_snp_venn()
    loci = paste0(input$a_select_venn_list_snp_loci,'GeneDf')
    venn = draw_genoppi_venn(diagram,  color = c('blue', 'red'), main =  '')
    grid::grid.newpage()
    grid::grid.draw(venn)
  })

  # get venn diagram text
  a_snp_venn_verbatim <- reactive({
    req(sigS(), a_snp_venn())
    pulldown = sigS()
    thresholds = paste(thldVals$sigTxt, thldVals$fcSigTxt, sep =', ')
    selected = input$a_select_venn_list_snp
    diagram = a_snp_venn()
    loci = input$a_select_venn_list_snp_loci
    loci = gsub('all','all loci',loci)
    loci = gsub('multi','multi-gene loci',loci)
    loci = gsub('single','single-gene loci',loci)
    A <- paste0("A = proteomic data subsetted by ", thresholds, " &#40;", bold(length(diagram[[1]])), "&#41;")
    B <- paste0("B = ",bold(selected)," genes mapped from ", loci,"&#40;", bold(length(unique(diagram[[2]]))), "&#41;")
    total <- paste0("Total population =", " &#40;", bold(nrow(pulldown)), "&#41;")
    return(list(A=A, B=B, total=total))
  })
  
  # print to ui
  output$a_snp_venn_verbatim_ui <- renderUI({
    output <- a_snp_venn_verbatim()
    HTML(paste(output$total, output$A, output$B, sep = "<br/>"))
  })
  
  ## GWAS catalog
  # subset all snps for gwas catalog
  a_gwas_catalogue_mapping_venn <- reactive({
    req(a_gwas_catalogue_mapping(), sigS())
    
    # get datasets
    pulldown = sigS()
    mapping = a_gwas_catalogue_mapping()
    mapping = subset_snp_loci(mapping)
    
    # only use all gene for GWAS cat
    loci = 'allGeneDf' # paste0(input$a_select_venn_gwas_catalogue_loci, 'GeneDf') 
    diagram = list(pulldown=pulldown[pulldown$significant,]$gene, genelist=mapping[[loci]]$gene)
    names(diagram) = c('A', 'B')
    return(diagram)
  })
  
  # gwas catalogue
  output$a_gwas_catalogue_venn_all_ui <- renderPlot({
    req(a_gwas_catalogue_mapping_venn)
    diagram = a_gwas_catalogue_mapping_venn()
    venn = draw_genoppi_venn(diagram,  color = c('blue', 'red'), main = '')
    grid::grid.newpage()
    grid::grid.draw(venn)
  })
  
  # get venn diagram text
  a_gwas_catalogue_venn_verbatim <- reactive({
    req(sigS(), a_gwas_catalogue_mapping_venn())
    pulldown = sigS()
    thresholds = paste(thldVals$sigTxt, thldVals$fcSigTxt, sep =', ')
    diagram = a_gwas_catalogue_mapping_venn()
    A <- paste0("A = proteomic data subsetted by ", thresholds, " &#40;", bold(length(diagram[[1]])), "&#41;")
    B <- paste0("B = Genes mapped from GWAS catalog &#40;", bold(length(unique(diagram[[2]]))), "&#41;")
    total <- paste0("Total population = proteomic data", " &#40;", bold(nrow(pulldown)), "&#41;")
    return(list(A=A, B=B, total=total))
  })
  
  # print to ui
  output$a_gwas_catalogue_venn_verbatim_ui <- renderUI({
    output <- a_gwas_catalogue_venn_verbatim()
    HTML(paste(output$total, output$A, output$B, sep = "<br/>"))
  })
  
  # hypergeometric overlap gnomAD
  a_gnomad_calc_hyper <- reactive({
    req(a_gnomad_sig_list(), sigS())
    
    # get data for overlap calculation
    pulldown = sigS()
    gnomad = data.frame(listName='gnomAD',a_gnomad_sig_list())
    intersect=data.frame(listName='gnomAD', intersectN=TRUE)
    
    # compile venn diagram information
    hyper = calc_hyper(pulldown, gnomad, intersect)
    hyper[['venn']][['A']] <- hyper$genes$gnomAD$success_genes # pulldown
    hyper[['venn' ]][['B']] <- hyper$genes$gnomAD$sample_genes # gnomAD genes
    return(hyper)
  })
  
  # draw venn diagram for gnomAD
  output$a_gnomad_venn_ui <- renderPlot({
    req(sigS(), a_gnomad_calc_hyper())
    hyper = a_gnomad_calc_hyper()
    v = draw_genoppi_venn(hyper$venn, color = c('blue','red'), main = paste0('P-value = ',format(hyper$statistics$pvalue, digits = 3)))
    grid::grid.newpage()
    grid::grid.draw(v)
  })
  
  # plot below venn diagram inweb
  a_gnomad_venn_verbatim <- reactive({
    req(sigS(), a_gnomad_calc_hyper())
    tresholds = paste(thldVals$sigTxt, thldVals$fcSigTxt, sep =', ')
    hyper = a_gnomad_calc_hyper()
    A <- paste0("A = proteomic data subsetted by ", tresholds, " &#40;", bold(hyper$statistics$success_count), "&#41;")
    B <- paste0("B = gnomAD genes with pLI ≥", bold(input$a_slide_gnomad_pli_threshold)," &#40;", bold(hyper$statistics$sample_count), "&#41;")
    total <- paste0("Total population = proteomic data &cap; gnomAD &#40;", bold(hyper$statistics$population_count), "&#41;")
    return(list(A=A, B=B, total=total))
  })
  
  # print to ui
  output$a_gnomad_venn_verbatim_ui <- renderUI({
    output <- a_gnomad_venn_verbatim()
    HTML(paste(output$total, output$A, output$B, sep = "<br/>"))
  })
  
  
  #---------------------------------------------------------------
  # gnomad plot clicking integration
  
  #a_table_gnomad_constraints <- reactive({
  #  hover_index = event_data("plotly_click", source = "Multi_VolcanoPlot")
  #  if (!is.null(hover_index)){
  #    if (hover_index$key %in% gnomad_table$gene){
  #      tabl = get_gnomad_constraints(hover_index$key)
  #      return(tabl)
  #    }
  #  }
  #})
  
  # render text for gnomad status
  #output$a_gnomad_constraints_available_ui <- renderUI({
  #  gene = event_data("plotly_click", source = "Multi_VolcanoPlot")$key
  #  if (!is.null(gene)){
  #    if (gene %in% gnomad_table$gene){
  #      return(HTML(paste(bold(gene),'constraint info from gnomAD 2.1.1.')))
  #    } else {
  #      return(HTML(paste('No constraint info for', bold(gene), 'in gnomAD 2.1.1.')))
  #    }
  #  }
  #  return('Nothing selected. Click a plot point.')
  #})
  
  # render table
  #output$a_table_gnomad_constraints_ui <- renderTable(a_table_gnomad_constraints())
  
  ## there is a dependency on this somewhere..do not remove for now.
  a_pf_db <- reactive({
    if(!is.null(input$a_pfam_db)){
      pf_db <- input$a_pfam_db
    }
  })
  

  

  #---------------------------------------------------------------------
  # integrated plotting
  
  # generate plot in ggformat
  a_integrated_plot_gg <- reactive({
    req(plotVals$volcano_basic)
    p = plotVals$volcano_basic
    
    if (!is.null(input$a_gwas_catalogue)) {
      # TODO add labelling for each chosen gwas catalogue
      if (any(input$a_gwas_catalogue != '') & input$a_overlay_gwas_cat) {
        p = plot_overlay(p, list(gwas=a_gwas_catalogue_mapping()))
      }
    } 
    if (!is.null(input$a_file_genes_rep)) if (input$a_overlay_genes_upload) {p = plot_overlay(p, list(upload=a_genes_upload()$data))}
    if (!is.null(input$a_file_SNP_rep)) if (input$a_overlay_snp) {p = plot_overlay(p, list(snps=a_snp_mapping()))}
    if (!is.null(ppiParams()$bait_search)) {
      if (ppiParams()$bait_search %in% c(inweb_table$Gene1,inweb_table$Gene2) &
          ppiParams()$overlay_inweb) {
        p = plot_overlay(p, list(inweb=a_ppi_mapping_df()))}}
    if (!is.null(input$a_overlay_gnomad)) {
      if (input$a_overlay_gnomad) {
        p = plot_overlay(p, list(gnomad=a_gnomad_mapping_threshold()))}}
    if (!is.null(input$a_tissue_select)) if (!is.null(a_get_tissue_list())) if (input$a_overlay_tissue) p = plot_overlay(p, list(tissuemap=a_tissue_mapping()))
    
    if (developer()) showNotification(paste(head(p$overlay, n = 1), collapse = ' '), duration = 10)
    
    # collapse/combine labels from multiple overlay
    if (!is.null(p$overlay)) p$overlay <- collapse_labels(p$overlay)
    p
    
  })
  
  # convert into plotly graphics
  a_integrated_plot <- reactive({
    sig_label = '(Significant)' #paste0(monitor_significance_thresholds()$sig) #, ', ', monitor_logfc_threshold_non_html()$sig)
    p <- a_integrated_plot_gg()
    p <- make_interactive(p, source = "Multi_VolcanoPlot", legend = T, sig_text = sig_label)
    p <- genoppi::add_plotly_threshold_lines (
      p, 
      line_pvalue = statsParamVals$pValThresh, 
      line_logfc = statsParamVals$logfcThresh, 
      logfc_direction = statsParamVals$logfcDir,  
      sig_type = statsParamVals$signifType)
    if (!is.null(goiS())) {
      p <- add_plotly_markers_search(
        p, goiS(), alpha = goiAlphaS())
    } 
    p <- add_plotly_layout_volcano(
      p, 
      width = global.integrated.volcano.width, 
      height = global.integrated.volcano.height) # error in searching overlay here when layout width/height supplied. 
    p
  })
  
  # download integrated plot graphics.
  input_integrated_plot_gg <- function(){a_integrated_plot_gg()}
  output$a_integrated_plot_download = downloadHandler(
    filename = 'genoppi-integrated-plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot =  input_integrated_plot_gg(), device = device, 
             width = global.img.volcano.download.width,
             height = global.img.volcano.download.height)
    })
  
 # download pathway annoation plot
 input_pathway_plot_gg <- function(){a_pathway_plot_gg()}
  output$a_pathway_plot_download = downloadHandler(
    filename = paste0('genoppi-','geneset', '-plot.png'),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot =  a_pathway_plot_gg(), device = device, 
             width = global.img.volcano.download.width,
             height = global.img.volcano.download.height)
    })
  
  # save the legend of the pathway plot
  input_pathway_plot_legend_gg <- function(){a_pathway_plot_legend_gg()}
  output$a_pathway_plot_legend_download = downloadHandler(
    filename = paste0('genoppi-', 'geneset', '-legend.png'),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = a_pathway_plot_legend_gg(), device = device, 
             width = global.img.volcano.download.width,
             height = global.img.volcano.download.height)
    })
  #---------------------------------------------------------------------
  # integrated plotting
  
  # assign frequency 
  a_pathway_mapping_assign_freq <- reactive({
    req(sigS(), input$a_pf_loc_option)
    pulldown <- sigS()
    validate(need(!all(is.na(pulldown$gene)), ""))
    db = input$a_pf_loc_option
    if (sum(pulldown$significant) > 0){
      overlap <- get_pathways(db, pulldown$gene[pulldown$significant])
      overlap <- assign_freq(overlap, 'pathway')
      overlap = merge(overlap, pulldown)
      return(overlap)
    } else return(NULL)
  })
  
  # load in data and preset colors in a seperate
  # reactive to reduce overhead time
  a_pathway_mapping_initial <- reactive({
    req(sigS(), a_pathway_mapping_assign_freq())
    
    #  # get raw data and assign frequency count
    overlap <- a_pathway_mapping_assign_freq()
    
    # setup color scheme
    colors = as.data.frame(overlap[,c('pathway','Freq')])
    colors = colors[!duplicated(colors), ]
    colors$color = NA
    set.seed(global.color.pathway.seed)
    n = sum(is.na(colors$color))
    colors$color[is.na(colors$color)] <- sample(color_distinct(n), n) 
    
    # merge with overlap
    overlap = merge(overlap, colors[,c('pathway','color')], by = 'pathway')
    overlap = overlap[overlap$significant, ]
    return(overlap)
  })
  
  # get frequency
  a_pathway_mapping_values <- reactive({
    req(a_pathway_mapping_initial())
    return(unique(a_pathway_mapping_initial()))
  })
  
  # calculate how many times a certain pathway appears
  a_pathway_mapping_freq_revcumsum <- reactive({
    req(a_pathway_mapping_initial())
    overlay = a_pathway_mapping_initial()
    revcumsum = calc_cumsum_table(overlay, col.id = 'pathway')
    return(revcumsum)
  })
  
  # calculate the lowest allowed frequency of pathway
  # that may appear in the plot
  a_pathway_mapping_freq_lowest_allowed <- reactive({
    req(a_pathway_mapping_freq_revcumsum())
    tabl = a_pathway_mapping_freq_revcumsum()
    lowest_allowed_freq = min(tabl$Freq[tabl$n <= max.genesets]) # see global.R
    return(lowest_allowed_freq)
  })
  
  # make data table
  output$a_pathway_data_table_ui <- DT::renderDataTable({
    req(a_pathway_mapping_initial())
    DT::datatable(a_pathway_mapping_initial()[,c('gene','pathway','Freq')])
  })
  
  # make mapping
  a_pathway_mapping <- reactive({
    req(a_pathway_mapping_initial())
    # get data and subset
    overlay = a_pathway_mapping_initial()
    if (nrow(overlay) > 0){
      overlay$dataset = overlay$pathway
      overlay$alt_label = overlay$pathway
      overlay$size = overlay$Freq/max(a_pathway_mapping_values()$Freq)
      overlay$size = 9+exp(3*overlay$size)
      overlay$gg.size = overlay$size/2 # deprectated name
      overlay$size_gg = overlay$size/2
      overlay$col_significant = overlay$color
      overlay$col_other = overlay$color
      overlay$symbol = 'square'
      overlay$shape = 22
      overlay$opacity = 0.8
      overlay$label = F
      return(overlay)
    } else {NULL}
  })
  
  # reactive for subsetting my frequency
  a_pathway_mapping_subset <- reactive({
    req(a_pathway_mapping(), sigS())
    
    # subset data by frequencies
    lowest_allowed_freq = a_pathway_mapping_freq_lowest_allowed()
    overlay = a_pathway_mapping()
    overlay = overlay[overlay$Freq >= lowest_allowed_freq,]
    overlay = overlay[overlay$Freq >= input$a_pathway_mapping_freq_slider[1] & 
                        overlay$Freq <= input$a_pathway_mapping_freq_slider[2], ]
  
    return(overlay)
  })
  
  # make the ggplot with legend
  a_pathway_plot_tmp_gg <- reactive({
    data = sigS()
    req(data, a_pathway_mapping_subset())
    req(plotVals$volcano_bait_overlay)
    p <- plotVals$volcano_bait_overlay
    if (sum(data$significant) > 0){
      if (nrow(a_pathway_mapping_subset()) > 0) {
        p <- plot_overlay(p, list(pathway=a_pathway_mapping_subset()), legend_nchar_max = max.nchar.legend)
      }
    }
    return(p)
  })
  
  # make the ggplot legend
  a_pathway_plot_legend_gg <- reactive({
    req(a_pathway_mapping_subset())
    
    # generate ggplot with \n 
    req(plotVals$volcano_bait_overlay)
    p <- plotVals$volcano_bait_overlay
    p <- plot_overlay(p, list(pathway=a_pathway_mapping_subset()), legend_nchar_max = max.nchar.legend, nchar_max_collapse = '\n')
    p <- p + theme(legend.key.height=unit(0.75, "cm"))
    
    # extract legend from plot
    req(p)
    legend = get_gg_legend(p)
    grid::grid.newpage()
    grid::grid.draw(legend) 
  
  })
  
  # make the ggplot with legend
  a_pathway_plot_tmp_gg <- reactive({
    data = sigS()
    req(data, a_pathway_mapping_subset())
    req(plotVals$volcano_bait_overlay)
    p <- plotVals$volcano_bait_overlay
    if (sum(data$significant) > 0){
      if (nrow(a_pathway_mapping_subset()) > 0) {
        p <- plot_overlay(p, list(pathway=a_pathway_mapping_subset()), legend_nchar_max = max.nchar.legend)
      }
    }
    return(p)
  })
  
  
  # remove the legend with ggplot
  a_pathway_plot_gg <- reactive({
    req(a_pathway_plot_tmp_gg())
    plt = a_pathway_plot_tmp_gg()
    req(plt)
    plt = plt + theme(legend.position = 'none')
    return(plt)
  })
  

  # convert to plotly
  a_pathway_plot <- reactive({
    req(sigS(), a_pathway_plot_gg(), input$a_pathway_mapping_type_sort)
  
    p <- a_pathway_plot_gg()
    
    # for now, we order overlay AFTER plot_overlay() has been called since 
    # the legend_order column would otherwise be disrupted through a merge operation.
    # see github issues for more details. In the future, this should be a reactive.
    if (!is.null(p$overlay$dataset)) {
      p$overlay$legend_order = unlist(
        ifelse(input$a_pathway_mapping_type_sort == 'freq',
               list(rev(order(p$overlay$size))), 
               list(order(p$overlay$dataset))))
    } 
    
    p <- make_interactive(p, legend = T)
    if (!is.null(goiS())) {
      p <- add_plotly_markers_search(
        p, goiS(), alpha = input$a_goi_search_rep_alpha) 
    }
    if (!is.null(input$a_pathway_mapping_search)) {
      p <- add_plotly_markers_search_pathway(
        p, input$a_pathway_mapping_search, mapping = a_pathway_mapping_initial())
    } 
    p <- genoppi::add_plotly_threshold_lines(
      p, 
      line_pvalue = statsParamVals$pValThresh, 
      line_logfc = statsParamVals$logfcThresh, 
      logfc_direction = statsParamVals$logfcDir,
      sig_type = statsParamVals$signifType)
    p <- add_plotly_layout_volcano(p, width = global.genesets.volcano.width, height = global.genesets.volcano.height)
    return(p)
  })
  

  a_multi_vp_plus <- reactive({
    validate(
      need(!is.null(a_search_gene()), "")
    )
    p <- a_multi_vp_layer()
    goi <- a_search_gene()
    orig_data <- dataS()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  output$VolcanoPlotPathway <- plotly::renderPlotly({
    req(a_pathway_plot())
    a_pathway_plot()
  })
  
  output$ScatterPlot <- plotly::renderPlotly({
    validate(need(a_file_pulldown_r()  != '', "Upload file"))
    a_sp()
  })

  output$Multi_VolcanoPlot <- plotly::renderPlotly({
    validate(need(dataPathS()  != '', "Upload file"))
    req(sigS())
    a_integrated_plot()
  })
  
  output$a_file_display_table_ui <- DT::renderDataTable({
    req(dataFrameS())
    DT::datatable(dataFrameS()$data)
  })
  
 
  ##### GENERAL #####
  #documentation
  output$info_gwas_ui <- renderUI({actionLink('info_gwas', ' ', icon = icon('question-circle'))})
  observeEvent(input$info_gwas,{
    text = paste(readLines(find_docs('gwas_table.info')))
    showModal(modalDialog(HTML(text), easyClose = T, footer=genoppi.ver, title = 'GWAS catalog'))
  })
  
  output$info_gnomad_ui <- renderUI({actionLink('info_gnomad', ' ', icon = icon('question-circle'))})
  observeEvent(input$info_gnomad,{
    text = paste(readLines(find_docs('gnomad_table.info')))
    showModal(modalDialog(HTML(text), easyClose = T, footer=genoppi.ver, title = 'gnomAD'))
  })
  
  output$info_tissue_ui <- renderUI({actionLink('info_tissue', ' ', icon = icon('question-circle'))})
  observeEvent(input$info_tissue,{
    text = paste(readLines(find_docs('hpa_rna.info')), '<br> <br>', 
                 readLines(find_docs('gtex_rna.info')), '<br> <br>', 
                 readLines(find_docs('gtex_protein.info')))
    showModal(modalDialog(HTML(paste(text)), easyClose = T, footer=genoppi.ver, title = 'GTEx (RNA/Protein) or HPA (RNA)'))
  })
  
  output$info_tissue_enrichment_ui <- renderUI({actionLink('info_tissue_enrichment', ' ', icon = icon('question-circle'))})
  observeEvent(input$info_tissue_enrichment,{
    text = paste(readLines(find_docs('hpa_rna.info')), '<br> <br>', 
                 readLines(find_docs('gtex_rna.info')), '<br> <br>', 
                 readLines(find_docs('gtex_protein.info')))
    showModal(modalDialog(HTML(paste(text)), easyClose = T, footer=genoppi.ver, title = 'GTEx (RNA/Protein) or HPA (RNA)'))
  })
  
  output$info_geneset_ui <- renderUI({actionLink('info_geneset', ' ', icon = icon('question-circle'))})
  observeEvent(input$info_geneset,{
    files = list.files('documentation/', full.names = T)
    files = files[grepl(files, pattern = '(msigdb)|(go)|(hgnc)')]
    text = paste(lapply(files, function(x) readLines(x)), collapse = '<br> <br>')
    showModal(modalDialog(HTML(paste(text)), easyClose = T, footer=genoppi.ver, title = 'Geneset annotations'))
  })

  getPage_guide<-function() {
    return(tags$iframe(src = "welcome_guide_200509.pdf",
                      style="width:100%;",  #frameborder="0"
                      height = "3100px"))
  }
  
  output$how_to_guide <- renderUI({
    getPage_guide()
  })
  
  output$documentation <- renderUI({
    return(includeHTML("documentation/doc_0316.html"))
  })
  
})