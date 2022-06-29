
# shiny server
shinyServer(function(input, output, session){
  #supress warnings
  storeWarn<- getOption("warn")
  options(warn = -1) 
  
  observe({
    input$filetype
    updateTabsetPanel(session, "basic", selected = "p1")
  })
  
  
  ##### VISUALIZATIONS START ##### 
  
  ## documentation tab
  
  output$documentation_ui <- renderText({
    # return(HTML(documentation))
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
    HTML(paste('Try a single',actionLink('a_get_example_file', 'example file'), 'or',
               actionLink('a_get_example_multiple_files', 'multiple example files'),'!'))
  })
  
  output$a_file <- renderUI({
    fileInput('a_file_pulldown_r', 'Upload user input file', accept = files_accepted)
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
    # Uncomment to get guide
    # updateTabItems(session, "sidebarmenu", 'guide')
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
  
  output$a_logfc_direction_ui <- renderUI({
    radioButtons("a_logfc_direction", HTML("log<sub>2</sub>FC direction"),
                 c("Neg" = "negative", 
                   "Both" = "both",
                   "Pos" = "positive"),
                 selected = 'positive',
                 inline = T)
  })
  
  
  # 
  output$a_significance_type_ui <- renderUI({
    radioButtons("a_significance_type", "Significance metric",
                 #c("FDR" = "fdr", "<i>P</i>-value" = "pvalue"),
                 choiceNames = list("FDR", HTML("<i>P</i>-value")),
                 choiceValues = list("fdr",'pvalue'),
                 inline = T)
  })
  
  output$FDR_thresh <- renderUI({
    #validate(need(input$a_significance_type == 'fdr', ''))
    sliderInput("a_fdr_thresh", "FDR threshold",
                min = 0, max = 1, value = 0.1, step = 0.01)
  })
  
  # TODO render select type of mod t-test
  select_mod_ttest_server("a_select_mod_ttest")
  
  output$PVal_thresh <- renderUI({
    sliderInput("a_pval_thresh", HTML("<i>P</i>-value threshold"),
                min = 0, max = 1, value = 0.05, step = 0.001)
  })
  
  # based on a_pulldown(), create slider for logFC
  output$logFC_thresh <- renderUI({
    if(!is.null(a_file_pulldown_r() )){
      limit <- calc_logfc_limit(a_pulldown(), input$a_logfc_direction)
      sliderInput("a_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                  min = 0, max = limit, value = 0, step = 0.1)
    }else
      sliderInput("a_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                  min = 0, max = 1, value = 0, step = 0.1)
  })  

  # track significance threshols for FDR and P-value
  monitor_significance_thresholds <- reactive({
    sig_type = ifelse(input$a_significance_type == 'fdr', 'FDR', '<i>P</i>-value')
    sig_value = ifelse(sig_type == 'FDR', input$a_fdr_thresh, input$a_pval_thresh)
    fc_sign = ifelse(input$a_logfc_direction, '<', '≥')
    region_le <- c(paste0(sig_type,"≤", sig_value))
    region_g <- c(paste0(sig_type,">", sig_value))
    return(list(sig=region_le, insig=region_g, sig_type=sig_type, sig_value=sig_value))
  })
  
  # track significance threshols for logFC
  monitor_logfc_threshold <- reactive({
    fc_dir = input$a_logfc_direction
    fc = input$a_logFC_thresh
    if (fc_dir == 'negative') {fc_sig = paste("log<sub>2</sub>FC&lt;", -fc); fc_insig =  paste("log<sub>2</sub>FC&ge;", -fc)}
    if (fc_dir == 'positive') {fc_sig = paste("log<sub>2</sub>FC&ge;", fc); fc_insig =  paste("log<sub>2</sub>FC&lt;", fc)}
    if (fc_dir == 'both') {fc_sig = paste("|log<sub>2</sub>FC|&ge;", fc); fc_insig = paste("|log<sub>2</sub>FC|&lt;", fc) }
    return(list(sig=fc_sig, insig=fc_insig))
  })

  output$a_sig_text_ui <- renderUI({
    HTML(paste0(monitor_significance_thresholds()$sig, ', ', monitor_logfc_threshold()$sig))
  })
  
  output$a_insig_text_ui <- renderUI({
    HTML(paste0(monitor_significance_thresholds()$insig, ', ', monitor_logfc_threshold()$insig))
  })
  
  
  #----------------------------------------------------------
  # sliders for selecting color, symbols and labels of plots

  # basic plot
  val_a_color_theme_indv_sig <- reactiveVal(value = '#41AB5D')
  observeEvent(input$a_color_indv_sig, { val_a_color_theme_indv_sig(input$a_color_indv_sig)})
  
  output$a_color_theme_indv_sig <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    label = (HTML(paste(c('Colors for ',monitor_significance_thresholds()$sig, 'and', monitor_logfc_threshold()$sig))))
    colourpicker::colourInput('a_color_indv_sig', label, value = val_a_color_theme_indv_sig(), showColour = 'both', 
                  palette = c( "limited"), allowedCols = allowed_colors)
  })
    
  # basic plot
  val_a_color_theme_indv_insig <- reactiveVal(value = '#808080')
  observeEvent(input$a_color_indv_insig, { val_a_color_theme_indv_insig(input$a_color_indv_insig)})
  
  output$a_color_theme_indv_insig <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    label = (HTML(paste(c('Colors for ',monitor_significance_thresholds()$insig, 'or', monitor_logfc_threshold()$insig))))
    colourpicker::colourInput('a_color_indv_insig', label, value = val_a_color_theme_indv_insig(), showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  
  # intgrated plot, snp
  output$a_color_snp_sig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_snp_sig', NULL, value = 'blue', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, snp
  output$a_color_snp_insig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_snp_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  
  # integrated plot, snp
  output$a_symbol_snp_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    selectInput('a_symbol_snp', NULL, choices = allowed_plotly_symbols, selected = 'square')
  })
  
  # integrated plot, snp
  output$a_label_snp_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_label_snp", label = "Toggle labels", value = TRUE)
  })
  
  # integrated plot, snp
  output$a_overlay_snp_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_overlay_snp", label = "Toggle overlay", value = TRUE)
  })
  
  # integrated plot, snp,
  output$a_reset_snp_ui <- renderUI({
    actionButton('a_reset_snp','clear')
  })
  
  
  # intgrated plot, genes upload
  output$a_color_genes_upload_sig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_genes_upload_sig', NULL, value = '#A52A2A', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, genes upload
  output$a_color_genes_upload_insig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_genes_upload_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  
  # integrated plot, genes uplaod
  output$a_symbol_genes_upload_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    selectInput('a_symbol_genes_upload', NULL, choices = allowed_plotly_symbols, selected = 'square')
  })
  
  # integrated plot, genes upload
  output$a_label_genes_upload_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_label_genes_upload", label = "Toggle labels", value = TRUE)
  })
  
  # integrated plot, genes upload
  output$a_overlay_genes_upload_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_overlay_genes_upload", label = "Toggle overlay", value = TRUE)
  })
  
  # integrated plot, reset
  output$a_reset_genes_upload_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    actionButton('a_reset_genes_upload', 'Reset')
  })
  observeEvent(input$a_reset_genes_upload, {
    reset("a_file_genes_rep")
  })
  
  
  # intgrated plot, inweb
  output$a_color_inweb_sig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_inweb_sig', NULL, value = 'yellow', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, inweb
  output$a_color_inweb_insig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_inweb_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # integrated plot, inweb
  output$a_symbol_inweb_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    selectInput('a_symbol_inweb', NULL, choices = allowed_plotly_symbols, selected = 'circle')
  })
  
  # integrated plot, inweb
  output$a_overlay_inweb_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_overlay_inweb", label = "Toggle overlay", value = TRUE)
  })
  
  # integrated plot, inweb
  output$a_label_inweb_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_label_inweb", label = "Toggle labels", value = TRUE)
  })
  
  
  # intgrated plot, gwas catalogue
  output$a_color_gwas_cat_sig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_gwas_cat_sig', NULL, value = 'cyan', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, gwas catalogue
  output$a_color_gwas_cat_insig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_gwas_cat_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  
  # integrated plot, gwas catalogue
  output$a_overlay_gwas_cat_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_overlay_gwas_cat", label = "Toggle overlay", value = TRUE)
  })
  
  # integrated plot, gwas catalogue
  output$a_symbol_gwas_cat_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    selectInput('a_symbol_gwas_cat', NULL, choices = allowed_plotly_symbols, selected = 'diamond')
  })
  # integrated plot, genes upload
  output$a_label_gwas_cat_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_label_gwas_cat", label = "Toggle labels", value = TRUE)
  })
  
  # intgrated plot, gnomad
  output$a_color_gnomad_sig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', ')))
    colourpicker::colourInput('a_color_gnomad_sig', NULL, value = '#FF00FF', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, gnomad
  output$a_color_gnomad_insig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #label = isolate(HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', ')))
    colourpicker::colourInput('a_color_gnomad_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, gnomad
  output$a_symbol_gnomad_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    selectInput('a_symbol_gnomad', NULL, choices = allowed_plotly_symbols, selected = 'circle')
  })
  
  # intgrated plot, gnomad
  output$a_label_gnomad_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_label_gnomad", label = "Toggle labels", value = FALSE)
  })
  
  # integrated plot, gnomad
  output$a_overlay_gnomad_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_overlay_gnomad", label = "Toggle overlay", value = FALSE)
  })

  # integrated plot, gnomad 
  output$a_select_gnomad_pli_type_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    radioButtons("a_select_gnomad_pli_type", label = 'Select pLI type', 
                 choiceNames = list('Threshold'),
                 choiceValues = list('threshold'))
  })
  
  # integrated plot, gnomad slider
  output$a_slide_gnomad_pli_threshold_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    #validate(need(input$a_select_gnomad_pli_type == 'threshold', ""))
    sliderInput(inputId = "a_slide_gnomad_pli_threshold", label = 'Subset interactors by pLI threshold', 
                min = 0, max = 1, value = 0.9, step = 0.01)
  })
  
  # intgrated plot, tissue
  output$a_color_tissue_sig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    colourpicker::colourInput('a_color_tissue_sig', NULL, value = '#3AFF00', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, tissue
  output$a_color_tissue_insig_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    colourpicker::colourInput('a_color_tissue_insig', NULL, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, tissue
  output$a_symbol_tissue_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    selectInput('a_symbol_tissue', NULL, choices = allowed_plotly_symbols, selected = 'circle')
  })
  
  # intgrated plot, tissue
  output$a_label_tissue_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_label_tissue", label = "Toggle labels", value = FALSE)
  })
  
  # integrated plot, tissue
  output$a_overlay_tissue_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    checkboxInput("a_overlay_tissue", label = "Toggle overlay", value = TRUE)
  })
  


  ## PPI databases
  
  output$a_bait_layer <- renderUI({
    textInput("a_bait_rep", value = "", "Input HGNC symbol to search for protein interactors (e.g. ZBTB7A)")
  })
  
  output$a_inweb_type <- renderUI({
    selectInput('a_inweb_type', 'Select interactor type', c("All" = 'all',"High-confidence" = 'hc',"Gold-standard" = 'gs'))
  })
  
  output$a_bioplex_type_ui <- renderUI({
    sliderInput('a_bioplex_type', 'Select bioplex probability', 0, 1, 0.9, 0.01)
  })
  
  output$a_irefindex_type_ui <- renderUI({
    sliderInput('a_irefindex_type', 'Select min. publications', min = 1, max = max(irefindex_table$Score.np.max), value = 2, step = 1)
  })
  
  output$a_bait_search <- renderUI({
    textInput("a_bait_search_rep", "Input bait (HGNC symbol, e.g. BCL2)")
  })
  
  a_bait_parsed <- reactive({
    bait = input$a_bait_search_rep
    if (bait == '') return(NULL)
    else return(bait)
  })
  
  output$a_GOI_search <- renderUI({
    textInput("a_goi_search_rep", "Search HGNC symbol")
  })
  
  output$a_GOI_search_alpha <- renderUI({
    shinyjs::hidden(sliderInput('a_goi_search_rep_alpha', 'Adjust alpha', min = 0, max = 1, value = 0.8, step = 0.05))
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
  
  # Search for replicates in data
  # DONE change to using enumerate_replicate_combinations for generating
  # available correlation plots
  available_replicates <- reactive({
    req(a_pulldown())
    d <- a_pulldown()
    return(enumerate_replicate_combinations(d))
  })
  
  # make summary of replicates
  replicate_summary_table <- reactive({
    req(a_sp_gg())
    p = a_sp_gg_all()
    rs = lapply(p, function(x) format(x$correlation, digits = 4))
    rs = t(data.frame(rs))
    rs = cbind(gsub('\\.',' vs ',rownames(rs)), rs)
    rs = rbind(rs, c('Average', format(mean(as.numeric(rs[,2])), digits = 4)))
    colnames(rs) <- c('Comparison','Correlation (r)')
    return(rs)
  })
  
  output$a_replicate_summar_text_ui <- renderUI({
    req(replicate_summary_table())
    h5(HTML(bold('Replicate correlation(s):')))
  })
  
  # render replicate summary
  output$a_replicate_summary_table_ui <- renderTable({
    replicate_summary_table()
  })
  
  # render select scatter plot
  output$a_select_scatterplot_ui <- renderUI({
    
    # rename reps
    
    rep_input = available_replicates()
    #if (!is.null(rep_input)) 
    
    reps_verbatim = gsub('rep','replicate ', rep_input)
    reps_verbatim = gsub('\\.', ' and ', reps_verbatim)
    reps = lapply(rep_input, function(x){x})
    names(reps) = reps_verbatim
    selectInput('a_select_scatterplot',
                'Replicates to compare in scatter plot', 
                choices = reps)
  })
  
  # TODO render select type of mod t-test
  # output$a_select_mod_ttest_ui <- renderUI({
  #   
  #   selectInput(
  #     'a_select_mod_ttest',
  #     'Select type of moderated t-test',
  #     choices = c("one sample mod t-test",
  #                 "two sample mod t-test")
  #   )
  # })
  
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
  
  # based on a_pulldown(), create slider for logFC
  output$a_logFC_slider <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    
    catf('deprecated..!')
    
    if(!is.null(a_pulldown())){
      input_file <- a_pulldown()
      df <- input_file
      min_logFC <- min(df$logFC)
      min_logFC <- round(min_logFC-0.5, 1)
      max_logFC <- max(df$logFC)
      max_logFC <- round(max_logFC+0.5, 1)
      sliderInput("a_logFC_range", "logFC",
                  min = min_logFC, max = max_logFC, value = c(0, max_logFC), step = 0.1)
    }
  })
  
  # based on a_pulldown(), create slider for logFC
  output$a_FDR_slider <- renderUI({
    validate(need(a_file_pulldown_r()  != '', ""))
    sliderInput("a_FDR_range", "FDR",
                min = 0, max = 1, value = c(0, 0.1), step = 0.01)
  })
  
  # based on a_pulldown(), create slider for logFC
  output$a_pvalue_slider <- renderUI({
    validate(
      need(a_file_pulldown_r()  != '', "")
    )
    sliderInput("a_pvalue_range", "pvalue",
                min = 0, max = 1, value = c(0, 1), step = 0.01)
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
    req(a_pulldown_significant())
    freq = a_pathway_mapping_values()$Freq
    fmax = ifelse(is.null(freq), 1, max(freq))
    fmin = ifelse(is.null(freq), 1, a_pathway_mapping_freq_lowest_allowed()) #ifelse(is.null(freq), 1, min(freq)) 
    sliderInput("a_pathway_mapping_freq_slider", "Subset by frequency",
                min = fmin, max = fmax, value = c(fmin, fmax), step = 1)
  })
  
  output$a_pathway_mapping_type_sort_ui <- renderUI({
    req(a_pulldown_significant())
    radioButtons('a_pathway_mapping_type_sort', 'Sort legend',
                 c('alphabetically' = 'alpha',
                   'frequency' = 'freq'), 
                 inline = T)
    
  })
  
  
  output$a_pathway_mapping_search_ui <- renderUI({
    req(a_pulldown_significant())
    mapping = a_pathway_mapping_values()
    mapping = mapping[rev(order(mapping$Freq)), ]
    selectInput('a_pathway_mapping_search', 'Search gene set', unique(mapping$pathway), multiple=T, selectize=TRUE, selected = "grey")
  })
  
  # show/hide the fdr/pvalue bar
  observeEvent(input$a_significance_type,{
    if (input$a_significance_type == 'fdr'){
        shinyjs::hide("a_pval_thresh")
        shinyjs::show("a_fdr_thresh")
      } else {
        shinyjs::show("a_pval_thresh")
        shinyjs::hide("a_fdr_thresh")
      }
  })
  
  # check pulldown input format for inconsistencies
  a_input_errors <- reactive({
    req(a_file_pulldown_r() )
    d <- read_input(a_file_pulldown_r() $datapath, sep = '\t')
    errs = get_shiny_errors(d$data)
    return(errs)
  })
  
  # check input
  output$a_in_pulldown_check_ui <- renderUI({
    output <- a_input_errors()
    if (output != '') stop(HTML(paste(output, sep = "<br/>")))
  })
  
  # returns boolean indicating whether mapping was done
  a_gene_mapping_bool <- reactive({
    req(a_in_pulldown())
    pulldown <- a_in_pulldown()
    return(pulldown$format$check$accession_rep | pulldown$format$check$accession_signif)
  })
  
  # returns boolean indicating whether rep1-2 is in the data
  a_file_rep_bool <- reactive({
    req(a_in_pulldown())
    check = a_in_pulldown()$format$check
    return(check$gene_rep | check$accession_rep)
  })
  
  # loading the data and getting the pulldown
  a_in_pulldown <- eventReactive(a_file_pulldown_r() ,{
    req(a_file_pulldown_r() )
    validate(need(a_input_errors() == '', ""))
    d <- read_input(a_file_pulldown_r() $datapath, sep = '\t')
    d
  })
  
  # map accession_numbers to gene ids if needed
  a_orig_pulldown <- reactive({
    pulldown <- a_in_pulldown()
    if (a_gene_mapping_bool()){
      pulldown$data <- map_gene_id(pulldown$data)
      }
    return(pulldown$data)
  })  
  
  # final pulldown formatted data.frame
  a_pulldown <- reactive({
    req(a_orig_pulldown(), a_in_pulldown())
    pulldown <- a_orig_pulldown()
    format <- a_in_pulldown()$format
    # TODO add option to choose one vs two sample mod t-test
    two_sample <- input$a_select_mod_ttest == "Two sample"
    
    
    # moderated t.test still needed
    if (format$check$gene_rep | format$check$accession_rep){
      
      # set allowed column names
      allowed = unlist(format$allowed[unlist(format$check)])
      allowed_cols = lapply(allowed, function(x) grepl(x, colnames(pulldown)))
      allowed_vec = apply(do.call(rbind, allowed_cols), 2, any)
      allowed_vec = allowed_vec | 'gene' %in% colnames(pulldown)
      
      # ensure moderated t.test is only calculated on allowed columns
      pulldown = pulldown[,colnames(pulldown)[allowed_vec]]
      # TODO added two_sample parameter to the calc_mod_ttest call
      result = calc_mod_ttest(pulldown, two_sample = two_sample) 
      return(result)
    }
    
    # pvalue, fdr is supplied from user
    else if (format$check$gene_signif | format$check$accession_signif){
      result = pulldown
      return(result)
    
    # no valid columns found. 
    } else {
      return(NULL)
    }

  })
  
  # id the enriched proteins
  a_pulldown_significant <- reactive({
    req(a_pulldown())
    d = a_pulldown()
    if (input$a_significance_type == 'fdr'){
      d1 = id_enriched_proteins(d, fdr_cutoff = input$a_fdr_thresh, logfc_dir = input$a_logfc_direction, 
                                logfc_cutoff = input$a_logFC_thresh)
    } else {
      d1 = id_enriched_proteins(d, fdr_cutoff = NULL, p_cutoff = input$a_pval_thresh, logfc_dir = input$a_logfc_direction, 
                                logfc_cutoff = input$a_logFC_thresh)
    }
  return(d1)
  })
  
  
  # monitor pulldown input, mapping and input
  a_monitor_pulldown <- reactive({
    req(a_pulldown_significant())
    
    # monitor of some columns were discarded
    pulldown <- a_in_pulldown()
    allowed = unlist(pulldown$format$allowed[unlist(pulldown$format$check)])
    allowed_cols = lapply(allowed, function(x) grepl(x, colnames(pulldown$data)))
    allowed_vec = apply(do.call(rbind, allowed_cols), 2, any)
    accepted = colnames(pulldown$data)[allowed_vec]
    discarded = colnames(pulldown$data)[!allowed_vec]
    
    # check if gene columns have synonyms in data
    synonyms = strsplit(pulldown$data$gene, split = '(\\;)|(\\|)')
    synonyms_bool = unlist(lapply(synonyms, length)) > 1
    synonym_example = pulldown$data$gene[synonyms_bool][1]
    
    # check for NAs in rows
    na_rows = sum(apply(pulldown$data, 1, function(x) any(is.na(x))))
    na_cols = apply(pulldown$data, 2, function(x) any(is.na(x)))
    
    # check if p-values are already -log10 transformed
    check_log_pvalues <- any(pulldown$data$pvalue > 1)
    
    # pre-rendered messages
    msg1 = paste0(bold('Error:'),' None of the inputted column names are allowed')
    msg2 = paste0(bold('Warning:'),' only ', length(accepted),'/',length(allowed_vec),' input column names were accepted.')
    msg3 = paste0('The following column names were invalid and discarded: ', italics(paste0(discarded, collapse = ', ')),'.')
    msg4 = paste0('See supplementary protocol for a description of allowed data inputs.')
    msg5 = paste0(bold('Warning: '), 'NA(s) were found in ', na_rows, ' row(s). Check column(s): ', paste(names(na_cols)[na_cols], collapse = ', '))
    msg6 = paste0(bold('Note:  '), sum(synonyms_bool),' rows contain synonyms in "gene" column, e.g. gene "',synonym_example,
                  '". This column should only contain a single gene-name.')
    msg7 = paste0(bold('Warning: '), 'It looks like you have already -log10 transformed your p-values. Please, use raw p-values to accurately display volcano plots.')
    
    # no valid cols
    msg = ''
    if (length(accepted) == 0){
      msg = paste(msg, msg1, msg4)
      #return(HTML(paste(msg1, msg4)))
    # enough valid but some invalid
    } else if (length(accepted) != length(allowed_vec)){
      msg = paste(msg, msg2, msg3, msg4)
      #return(HTML(paste(msg2, msg3, msg4)))
    } 
    if (na_rows > 0){
      msg = paste(msg, msg5)
    }
    if (sum(synonyms_bool) > 0){
      msg = paste(msg, msg6)
    }
    if (check_log_pvalues){
      msg = paste(msg, msg7)
    }
    
    
    if (msg != '') return(HTML(msg))
    else return(NULL)
    
  })
 
  a_monitor_pulldown_mapping <- reactive({
    req(a_orig_pulldown())
    
    # mapping failed
    pulldown_mapping = a_orig_pulldown()
    failed = pulldown_mapping$accession_number[is.na(pulldown_mapping$gene)]
    absolute = paste0(length(failed),'/',nrow(pulldown_mapping))
    fraction = paste0(format(100*length(failed)/nrow(pulldown_mapping), digits = 3),'%')
  
    # messages
    msg0 = bold(paste('ERROR: ', absolute, ' (',fraction,') accesion_numbers were not mapped to a genes. The App may crash during Integrated Plotting!'))
    msg1 = paste0(bold('Warning:'), absolute, ' (',fraction,') accesion_number(s) were not mapped to a gene(s).')
    msg2 = paste0('The following accesion_number(s) were not mapped:', italics(paste0(failed,collapse=', ')),'.')
    msg3 = paste0('These will be ignored in downstream analysis. To include, manually specify the entry in a seperate "gene" (HGNC) column.')
    msg4 = paste0('Are you using human accession numbers?')
    
    if (length(failed) > 0){
      
      # if more than 99% are unmapped give a warning:
      if (length(failed)/nrow(pulldown_mapping) > 0.99){
        return(HTML(paste(msg0, msg4, msg2)))
      
        # otherwise, print out mapping
      } else {
        return(HTML(paste(msg1, msg2, msg3)))
      }

    } else return(NULL)
    
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
  
  # needed for searching gene
  a_search_gene <- reactive({
    gene_in <- input$a_goi_search_rep
    req(gene_in)
    if (gene_in != ''){
      toupper(gene_in)
    } else {
      return(NULL)
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
    mapping = get_snp_lists(infile = a_snp(), a_pulldown()$gene)
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
    req(input$a_bait_rep, a_pulldown(), input$a_ppi_select)
    mapping = NULL
    db = input$a_ppi_select
    if (db == 'inweb') {if (!is.null(input$a_inweb_type)) mapping = get_inweb_list(input$a_bait_rep, type = input$a_inweb_type)}
    if (db == 'bioplex') {if (!is.null(input$a_bioplex_type)) mapping = get_bioplex_list(input$a_bait_rep, p = input$a_bioplex_type)}
    if (db == 'irefindex') {if (!is.null(input$a_irefindex_type)) mapping = get_irefindex_list(input$a_bait_rep, n = input$a_irefindex_type)}
    return(mapping)
  })
  
  # keep track of name of PPI database, and add lower/upper case
  a_ppi_mapping_name <- reactive({
    req(a_ppi_mapping())
    name = NULL
    db = input$a_ppi_select
    if (db == 'inweb') name = 'Inweb'
    if (db == 'bioplex') name = 'Bioplex'
    if (db == 'irefindex') name = 'IRefIndex'
    return(name)
  })
  
  # map inweb proteins
  a_ppi_mapping_df <- reactive({
    req(input$a_bait_rep, a_pulldown(), a_ppi_mapping())
    mapping = a_ppi_mapping()
    #mapping = get_inweb_list(input$a_bait_rep, type = input$a_inweb_type)
    if (!is.null(mapping)){
        mapping = mapping[mapping$significant, ]
        mapping$col_significant = input$a_color_inweb_sig
        mapping$col_other = input$a_color_inweb_insig
        mapping$shape = symbol_to_shape(input$a_symbol_inweb)
        mapping$symbol = input$a_symbol_inweb
        mapping$label = input$a_label_inweb
        mapping$dataset = a_ppi_mapping_name()
        return(mapping)
    } 
  })
  
  # map gwas catalouge
  a_gwas_catalogue_mapping <- reactive({
    req(input$a_gwas_catalogue, a_pulldown())
    genes = a_pulldown()$gene
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
    genes = as.character(a_pulldown()$gene)
    
    # map genes to snps and find gwas table entry
    tabl = lapply(genes, function(x) gwas_table$SNP %in% genes_snps[[x]])
    tabl_bool = apply(do.call(cbind, tabl), 1, any)
    tabl_subset = gwas_table[tabl_bool, ]
    result = tabl_subset$DISEASE.TRAIT

    return(unique(result))
  })
  
  # setup main gnomad mapping
  a_gnomad_mapping <- reactive({
    req(a_pulldown(), input$a_slide_gnomad_pli_threshold)
    pulldown = a_pulldown_significant()
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
    req(a_pulldown(), input$a_slide_gnomad_pli_threshold)
    pulldown = a_pulldown_significant()
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
    req(a_pulldown_significant(), selected)
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
    req(a_pulldown_significant(), a_get_tissue_list())
    pulldown = a_pulldown_significant()
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
    req(a_pulldown_significant(), input$a_tissue_enrichment_type_select)
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
    req(a_pulldown_significant(), a_tissue_enrichment_table())
    pulldown = a_pulldown_significant()
    table = a_tissue_enrichment_table()
    enrichment = lapply_calc_hyper(pulldown, table)
    enrichment$log10pvalue <- -log10(enrichment$pvalue)
    enrichment$log10qvalue <- -log10(enrichment$BH.FDR) 
    enrichment$bhfdr <- enrichment$BH.FDR
    return(enrichment)
  })
  
  # controls what should be returned to enrichment plot
  a_tissue_enrichment_layout <- eventReactive(input$a_button_plot_tissue_enrichment,{
    req(a_pulldown_significant(), input$a_tissue_enrichment_xaxis, input$a_tissue_enrichment_slider)
    
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
  
  # download inweb mapping
  output$a_ppi_mapping_df_download <- downloadHandler(
    filename = function() {
      paste("genoppi-inweb-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = a_pulldown_significant()
      inweb = a_ppi_mapping_df()[,c("dataset","gene")]
      mymerge = merge(pulldown, inweb, by = 'gene')
      write.csv(mymerge, file, row.names = F)
    }
  )
   
  # download moderated t-test
  output$a_mttest_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-proteomic-results",".csv", sep="")
    },
    content = function(file) {
      write.csv(a_pulldown_significant(), file, row.names = F)
    }
  )
  
  # download gene upload mapping
  output$a_gene_upload_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-gene-upload-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = a_pulldown_significant()
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
      pulldown = a_pulldown_significant()
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
      pulldown = a_pulldown_significant()
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
      pulldown = a_pulldown_significant()
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
      pulldown = a_pulldown_significant()
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
      pulldown = a_pulldown_significant()
      pathway = a_pathway_mapping()[,c('gene','pathway','Freq')]
      mymerge = merge(pulldown, pathway, by = 'gene')
      mymerge$database = input$a_pf_loc_option
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  # # # venn diagrams # # #
  
  # venn diagram inweb mapping for inweb
  output$a_inweb_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("genoppi-inweb-venn-mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_inweb_calc_hyper()$venn
      write.csv(venn_to_table(venn), file, row.names = F)
   }
  )
  
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
  observeEvent(a_file_pulldown_r() , {shinyjs::toggle(id="a_mttest_mapping_download", condition=!is.null(a_file_pulldown_r() ))})
  observeEvent(input$a_bait_rep, {shinyjs::toggle(id="a_ppi_mapping_df_download", condition=!is.null(a_pulldown_significant()) & any(input$a_bait_rep %in% c(inweb_table$Gene1,inweb_table$Gene2)))})
  observe({shinyjs::toggle(id="a_snp_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_file_SNP_rep$datapath))})
  observe({shinyjs::toggle(id="a_gene_upload_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_file_genes_rep))})
  observe({shinyjs::toggle(id="a_gwas_catalogue_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_gwas_catalogue))})
  observe({shinyjs::toggle(id="a_gnomad_mapping_download", condition=!is.null(a_pulldown_significant()) )})
  #observe({shinyjs::toggle(id="a_tissue_mapping_download", condition=!is.null(a_pulldown_significant() & !is.null(a_tissue_mapping())))}) # this seems to cause GCP to crash?
  observe({shinyjs::toggle(id="a_pathway_mapping_download", condition=!is.null(a_pulldown_significant()))})
  observe({shinyjs::toggle(id="a_tissue_enrichment_download", condition=!is.null(a_tissue_enrichment()))})  

  # venn diagrams
  observeEvent(input$a_bait_rep, {shinyjs::toggle(id="a_inweb_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & any(input$a_bait_rep %in% c(inweb_table$Gene1,inweb_table$Gene2)))})
  observe({shinyjs::toggle(id="a_snp_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_file_SNP_rep$datapath))})
  observe({shinyjs::toggle(id="a_genes_upload_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_file_genes_rep))})
  observe({shinyjs::toggle(id="a_gwas_catalogue_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_gwas_catalogue))})
  observe({shinyjs::toggle(id="a_gnomad_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(a_gnomad_mapping_threshold()) )})
  observe({shinyjs::toggle(id="a_tissue_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_tissue_select) & !is.null(input$a_gtex_rna_tissue) )})
  
  # show hide select buttons (HPA/GTEx)
  observe({shinyjs::toggle(id="a_hpa_rna_tissue", condition = input$a_tissue_select == 'HPA - RNA')})
  observe({shinyjs::toggle(id="a_gtex_rna_tissue", condition = input$a_tissue_select == 'GTEx - RNA')})
  observe({shinyjs::toggle(id="a_gtex_protein_tissue", condition = input$a_tissue_select == 'GTEx - Protein')})
  observe({shinyjs::toggle(id="a_tissue_enrichment_type_select", condition=!is.null(a_pulldown_significant()) & input$a_tissue_select_source == 'genoppi')})
  observe({shinyjs::toggle(id="a_tissue_enrichment_upload", condition=!is.null(a_pulldown_significant()) & input$a_tissue_select_source == 'upload')})
  
  # show hide select PPI DBs
  observe({shinyjs::toggle(id="a_inweb_type", condition = input$a_ppi_select == 'inweb')})
  observe({shinyjs::toggle(id="a_bioplex_type", condition = input$a_ppi_select == 'bioplex')})
  observe({shinyjs::toggle(id="a_irefindex_type", condition = input$a_ppi_select == 'irefindex')})  
  
  # show hide alpha sliders
  observe({shinyjs::toggle(id="a_goi_search_rep_alpha", condition = input$a_goi_search_rep != '')})
  observe({shinyjs::toggle(id="b_goi_search_rep_alpha", condition = input$b_goi_search_rep != '')})
  
  # show/hide plot download buttons
  observeEvent(!is.null(a_pulldown_significant()),{
    #shinyjs::show("a_tissue_select_source")
    #shinyjs::show("a_tissue_enrichment_type_select")
    shinyjs::show("a_volcano_plot_download")
    shinyjs::show("a_scatter_plot_download")
    shinyjs::show("a_integrated_plot_download")
    shinyjs::show("a_pathway_plot_download")
    shinyjs::show("a_pathway_plot_legend_download")
  })
  
  #--------------------------------------------------------
  # venn digrams and hypergeometric testing
  
  # INWEB
  # inweb hypergeometric overlap
  a_inweb_calc_hyper <- reactive({
    req(input$a_bait_rep, a_pulldown_significant(), a_ppi_mapping(), a_ppi_mapping_name())
    
    #inweb_output = get_inweb_list(input$a_bait_rep)
    mapping_output = a_ppi_mapping()
    dbname = a_ppi_mapping_name()
    
    if (!is.null(mapping_output)){
      
      # gather all ppi data
      ppi_list = data.frame(listName=dbname, mapping_output)
      ppi_intersect = data.frame(listName=dbname, intersectN=T)
      data = a_pulldown_significant()
      
      # compile venn diagram information
      hyper = calc_hyper(data, ppi_list, ppi_intersect, bait = a_bait_parsed())
      hyper[['venn']][['A']] <- hyper$genes[[dbname]]$success_genes # pulldown
      hyper[['venn' ]][['B']] <- hyper$genes[[dbname]]$sample_genes # ppi 
      return(hyper)
    } else {NULL}
  })
  
  # draw venn diagram
  output$a_inweb_venn_ui <- renderPlot({
    req(input$a_bait_rep, a_pulldown_significant(), a_inweb_calc_hyper())
    hyper = a_inweb_calc_hyper()
    v = draw_genoppi_venn(hyper$venn, 
                          color = c('blue','yellow'),#c(input$a_color_indv_sig, input$a_color_inweb_sig), 
                          main = paste0('P-value = ', format(hyper$statistics$pvalue, digits = 3)))
    grid::grid.newpage()
    grid::grid.draw(v)
  })
  
  # plot below venn diagram inweb
  a_ppi_venn_verbatim <- reactive({
    req(a_pulldown_significant(), a_inweb_calc_hyper(), input$a_bait_rep, a_ppi_mapping_name())
    thresholds = paste(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig, sep =', ')
    db = a_ppi_mapping_name()
    hyper = a_inweb_calc_hyper()
    A <- paste0("A = proteomic data subsetted by ", thresholds, " &#40;", bold(hyper$statistics$success_count), "&#41;")
    B <- paste0("B = ", bold(input$a_bait_rep)," ", db," interactors", " &#40;", bold(hyper$statistics$sample_count), "&#41;")
    total <- paste0("Total population = proteomic data &cap; ", db," &#40;", bold(hyper$statistics$population_count), "&#41;")
    return(list(A=A, B=B, total=total))
  })
  
  # message if Inweb can not be found
  output$a_ppi_message <- renderUI({
    req(input$a_bait_rep, a_pulldown(), input$a_ppi_select)
    
    # get info about current selection
    query = input$a_bait_rep
    mapping = a_ppi_mapping()
    data = a_pulldown_significant()
    
    # send message to UI
    if (is.null(mapping)){
      return(HTML(paste(query, 'was not found in the database!')))
    } else if (query %nin% data$gene){
      return(HTML(paste(query,'was not found in proteomic data.')))
    }
  })
  
  output$a_inweb_venn_table_ui <- reactive({
    req(a_pulldown_significant(), a_inweb_calc_hyper())
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
    req(a_genes_upload(), a_pulldown_significant(), input$a_select_venn_list_genes_upload)
    
    # get data for overlap calculation
    pulldown = a_pulldown_significant()
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
    req(a_pulldown_significant(), a_genes_upload_venn(), input$a_select_venn_list_genes_upload)
    selected = input$a_select_venn_list_genes_upload
    pulldown = a_pulldown_significant()
    thresholds = paste(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig, sep =', ')
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
    req(input$a_tissue_select, a_pulldown_significant())
    output = a_get_tissue_list() 
    if (!is.null(output)){
      # setup data for calculating hypergeom. P-value.
      listname = toupper(input$a_tissue_select)
      output_list = data.frame(listName = listname, output)
      output_intersect = data.frame(listName = listname, intersectN = T)
      data = a_pulldown_significant()
      # compile venn diagram information
      hyper = calc_hyper(data, output_list, output_intersect, bait = NULL) #a_bait_parsed())
      hyper[['venn']][['A']] <- hyper$genes[[listname]]$success_genes # pulldown
      hyper[['venn' ]][['B']] <- hyper$genes[[listname]]$sample_genes # inweb
      return(hyper)
    } else {NULL}
  })
  
  # draw venn diagram
  output$a_tissue_venn_ui <- renderPlot({
    req(input$a_tissue_select, a_pulldown_significant(), a_tissue_calc_hyper())
    hyper = a_tissue_calc_hyper()
    v = draw_genoppi_venn(hyper$venn, color = c('blue','red'),
                          main = paste0('P-value = ', format(hyper$statistics$pvalue, digits = 3)))
    grid::grid.newpage()
    grid::grid.draw(v)
  })
  
  
  # text to be displayed alongside venn diagram
  a_tissue_venn_verbatim <- reactive({
    req(a_pulldown_significant(), a_tissue_calc_hyper(), input$a_tissue_select)
    
    # get text to be displayed
    thresholds = paste(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig, sep =', ')
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
    req(a_pulldown_significant(), input$a_select_venn_list_snp, a_snp_mapping())
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
    req(a_snp_draw_venn(), a_pulldown_significant(), input$a_select_venn_list_snp_loci)
    
    # variables and data for drawing venn
    loci = paste0(input$a_select_venn_list_snp_loci,'GeneDf')
    snplist = input$a_select_venn_list_snp
    pulldown = a_pulldown_significant()
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
    req(a_pulldown_significant(), a_snp_venn())
    pulldown = a_pulldown_significant()
    thresholds = paste(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig, sep =', ')
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
    req(a_gwas_catalogue_mapping(), a_pulldown_significant())
    
    # get datasets
    pulldown = a_pulldown_significant()
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
    req(a_pulldown_significant(), a_gwas_catalogue_mapping_venn())
    pulldown = a_pulldown_significant()
    thresholds = paste(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig, sep =', ')
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
    req(a_gnomad_sig_list(), a_pulldown_significant())
    
    # get data for overlap calculation
    pulldown = a_pulldown_significant()
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
    req(a_pulldown_significant(), a_gnomad_calc_hyper())
    hyper = a_gnomad_calc_hyper()
    v = draw_genoppi_venn(hyper$venn, color = c('blue','red'), main = paste0('P-value = ',format(hyper$statistics$pvalue, digits = 3)))
    grid::grid.newpage()
    grid::grid.draw(v)
  })
  
  # plot below venn diagram inweb
  a_gnomad_venn_verbatim <- reactive({
    req(a_pulldown_significant(), a_gnomad_calc_hyper())
    tresholds = paste(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig, sep =', ')
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

  ## ggplot automatically generated and reactive color bars
  a_vp_colorbar <- reactive({
    req(input$a_color_indv_sig, input$a_color_indv_insig)
    thresholds = monitor_significance_thresholds()
    
    # generate matrix with colors
    d <- data.frame(limit = rep('x', 101), value = seq(0, 1, 0.01))
    if (thresholds$sig_type == 'FDR'){
      d$col <- ifelse(d$value < input$a_fdr_thresh, input$a_color_indv_sig, input$a_color_indv_insig)
    } else {
      d$col <- ifelse(d$value < input$a_pval_thresh, input$a_color_indv_sig, input$a_color_indv_insig)
    }

    # plot result
    bar <- ggplot(d, aes(xmin = 0, xmax = 0.1, ymin = d$value-0.01, ymax = d$value)) + geom_rect(fill = d$col) +      
      scale_y_continuous(trans = "reverse", breaks = seq(0, 1, 0.1)) +
      labs(title = ifelse(thresholds$sig_type == 'P-value', '  P', 'FDR')) + theme_genoppi_bar() + coord_fixed()
    bar
  })
  
  # basic volcano plot
  a_vp_gg <- reactive({
    req(a_pulldown_significant())
    d <- a_pulldown_significant()
    req(input$a_color_indv_sig, input$a_color_indv_insig)
    p <- plot_volcano_basic(d, col_significant = input$a_color_indv_sig, col_other = input$a_color_indv_insig)
    if (!is.null(a_bait_parsed())) p <- plot_overlay(p, as.bait(a_bait_parsed())) # add bait
    
    return(p)
  })
  
  # basic volcano plot
  a_vp_layerx <- reactive({
    req(a_vp_gg(), input$a_pval_thresh, input$a_logFC_thresh, input$a_logfc_direction, input$a_significance_type)
    p <- a_vp_gg()
    p <- make_interactive(p, legend = T)
    if (input$a_goi_search_rep != '') p <- add_plotly_markers_search(p, a_search_gene(), alpha = input$a_goi_search_rep_alpha)
    p <- genoppi::add_plotly_threshold_lines (p, line_pvalue = input$a_pval_thresh, line_logfc = input$a_logFC_thresh, logfc_direction = input$a_logfc_direction, sig_type = input$a_significance_type)
    p <- add_plotly_layout_volcano(p, width = global.basic.volcano.width, height = global.basic.volcano.height)
    
    return(p)
  })
  
  
  # basic plot gene count summary
  a_verbatim_count <- reactive({
    d <- a_pulldown_significant()
    HTML(paste(bold(sum(d$significant)), 'out of', bold(nrow(d)), 'proteins significant.'))
  })
  
  # basic plot significance text
  a_vp_count_text <- reactive({
    if(!is.null(a_verbatim_count())){
      enriched <- paste(c('Significance threshold:',monitor_significance_thresholds()$sig, 'and', monitor_logfc_threshold()$sig))
      HTML(enriched)
    }
  })
 
  a_sp_gg_all <- reactive({
    # handle all scatter plots
    req(a_pulldown_significant())
    validate(need(a_file_rep_bool() == TRUE , ""))
    d = a_pulldown_significant()
    p = plot_scatter_basic_all(d, col_significant = input$a_color_indv_sig, col_other = input$a_color_indv_insig)
    return(p)
  })
  
  a_sp_gg <- reactive({
  
    # what replicates are inputted
    req(input$a_select_scatterplot, a_pulldown_significant())
    rep = unlist(strsplit(input$a_select_scatterplot,'\\.'))
    p = a_sp_gg_all()

    # handle individual plot
    p1 = p[[input$a_select_scatterplot]]$ggplot
    if (!is.null(a_bait_parsed())) p1 = plot_overlay(p1, as.bait(a_bait_parsed()))
    p1$r = format(p[[input$a_select_scatterplot]]$correlation, digits = 3)
    p1
  
  })
  
  a_sp <- reactive({
    
    # get basic stats
    p1 = a_sp_gg()
    #rep = unlist(strsplit(input$a_select_scatterplot,'\\.'))
    r = p1$r
    
    # convert into interactive graphics
    p1 = make_interactive(p1)
    if (input$a_goi_search_rep != '') p1 <- add_plotly_markers_search(p1, a_search_gene(), alpha = input$a_goi_search_rep_alpha)
    p1 = add_plotly_layout_scatter(p1, paste0('r=',r), 
                                   width = global.basic.scatter.width, 
                                   height = global.basic.scatter.height,
                                   orientation = 'v')
    p1 = add_plotly_line_unity(p1)
    p1 %>%  layout(legend=list(yanchor="right", x = 1, y = 1))
    #p1 = add_line_lm(p1, x=rep[1], y=rep[2])
    
  })
  

  #---------------------------------------------------------------------
  # integrated plotting
  
  # generate plot in ggformat
  a_integrated_plot_gg <- reactive({
    p = a_vp_gg()
    
    if (!is.null(input$a_gwas_catalogue)) if (input$a_gwas_catalogue != '' & input$a_overlay_gwas_cat) p = plot_overlay(p, list(gwas=a_gwas_catalogue_mapping()))
    if (!is.null(input$a_file_genes_rep)) if (input$a_overlay_genes_upload) {p = plot_overlay(p, list(upload=a_genes_upload()$data))}
    if (!is.null(input$a_file_SNP_rep)) if (input$a_overlay_snp) {p = plot_overlay(p, list(snps=a_snp_mapping()))}
    if (!is.null(input$a_bait_rep)) if (input$a_bait_rep %in% c(inweb_table$Gene1,inweb_table$Gene2) & input$a_overlay_inweb) p = plot_overlay(p, list(inweb=a_ppi_mapping_df()))
    if (!is.null(input$a_overlay_gnomad)) if (input$a_overlay_gnomad) p = plot_overlay(p, list(gnomad=a_gnomad_mapping_threshold()))
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
    p <- genoppi::add_plotly_threshold_lines (p, line_pvalue = input$a_pval_thresh, line_logfc = input$a_logFC_thresh, logfc_direction = input$a_logfc_direction,  sig_type = input$a_significance_type)
    if (input$a_goi_search_rep != '') p <- add_plotly_markers_search(p, a_search_gene(), alpha = input$a_goi_search_rep_alpha)
    p <- add_plotly_layout_volcano(p, width = global.integrated.volcano.width, height = global.integrated.volcano.height) # error in searching overlay here when layout width/height supplied. 
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
  
  
  # download basic scatter plot
  input_sp_gg <- function(){a_sp_gg()}
  output$a_scatter_plot_download = downloadHandler(
    filename = 'genoppi-scatter-plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = theme_scatter(input_sp_gg()) , device = device, 
             width = global.img.scatter.download.width,
             height = global.img.scatter.download.height)
    })
  
  # download basic scatter plot
  input_vp_gg <- function(){a_vp_gg()}
  output$a_volcano_plot_download = downloadHandler(
    filename = 'genoppi-volcano-plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot =  theme_volcano(input_vp_gg()), device = device, 
             width = global.img.volcano.download.width,
             height = global.img.volcano.download.height)
    })
  
  
  

  #---------------------------------------------------------------------
  # integrated plotting
  
  # assign frequency 
  a_pathway_mapping_assign_freq <- reactive({
    req(a_pulldown_significant(), input$a_pf_loc_option)
    pulldown <- a_pulldown_significant()
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
    req(a_pulldown_significant(), a_pathway_mapping_assign_freq())
    
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
    req(a_pathway_mapping(), a_pulldown_significant())
    
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
    data = a_pulldown_significant()
    req(data, a_pathway_mapping_subset())
    p <- a_vp_gg()
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
    p <- a_vp_gg()
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
    data = a_pulldown_significant()
    req(data, a_pathway_mapping_subset())
    p <- a_vp_gg()
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
    req(a_pulldown_significant(), a_pathway_plot_gg(), input$a_pathway_mapping_type_sort)
  
    p <- a_pathway_plot_gg()
    
    # for now, we order overlay AFTER plot_overlay() has been called since 
    # the legend_order column would otherwise be disrupted through a merge operation.
    # see github issues for more details. In the future, this should be a reactive.
    if (!is.null(p$overlay$dataset)) p$overlay$legend_order = unlist(ifelse(input$a_pathway_mapping_type_sort == 'freq',
                                    list(rev(order(p$overlay$size))), list(order(p$overlay$dataset))))
    
    p <- make_interactive(p, legend = T)
    if (input$a_goi_search_rep != '') p <- add_plotly_markers_search(p, a_search_gene(), alpha = input$a_goi_search_rep_alpha)
    if (!is.null(input$a_pathway_mapping_search)) p <- add_plotly_markers_search_pathway(p, input$a_pathway_mapping_search, mapping = a_pathway_mapping_initial())
    p <- genoppi::add_plotly_threshold_lines (p, line_pvalue = input$a_pval_thresh, line_logfc = input$a_logFC_thresh, logfc_direction = input$a_logfc_direction, sig_type = input$a_significance_type)
    p <- add_plotly_layout_volcano(p, width = global.genesets.volcano.width, height = global.genesets.volcano.height)
    return(p)
  })
  

  a_multi_vp_plus <- reactive({
    validate(
      need(!is.null(a_search_gene()), "")
    )
    p <- a_multi_vp_layer()
    goi <- a_search_gene()
    orig_data <- a_pulldown()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  # plot colors bars
  output$FDR_colorbar <- renderPlot({
    validate(need(a_file_pulldown_r()  != '', ""))
    a_vp_colorbar()
  })
  
  output$FDR_colorbar_integrated <- renderPlot({
    validate(need(a_file_pulldown_r()  != '', ""))
    a_vp_colorbar()
  })
  

  # the actual volcano plot outputted to the user
  output$VolcanoPlot <- plotly::renderPlotly({
    validate(need(a_file_pulldown_r()  != '', "Upload file"))
      a_vp_layerx()
  })
  
  output$VolcanoPlotPathway <- plotly::renderPlotly({
    req(a_pathway_plot())
    a_pathway_plot()
  })
  
  
  output$a_verbatim_count_ui <- renderUI({
    validate(need(a_file_pulldown_r()  != '', " "))
    a_verbatim_count()
  })
  
  output$VP_count_text <- renderUI({
    validate(need(a_file_pulldown_r()  != '', " "))
    output <- a_vp_count_text()
    HTML(output)
  })
  
  output$a_monitor_pulldown_ui <- renderUI({
    req(a_monitor_pulldown())
    return(a_monitor_pulldown())
  })
  
  output$a_monitor_pulldown_mapping_ui <- renderUI({
    req(a_monitor_pulldown_mapping())
    return(a_monitor_pulldown_mapping())
  })
  
  
  output$ScatterPlot <- plotly::renderPlotly({
    validate(need(a_file_pulldown_r()  != '', "Upload file"))
    a_sp()
  })
  
  # initialize reactive values 
  sigColorS <- sigColorServer("sig_color")
  insigColorS <- insigColorServer("insig_color")
    
  # TODO comment out above
  dataPathS <- dataPathServer("data")
  dataFrameS <- dataFrameServer("data", dataPathS)
  extractColsS <- extractColumnsServer("metadata", dataFrameS)
  mapAccessionToGeneS <- mapAccessionToGeneServer("data", dataFrameS)
  statsParamsS <- statsParamsServer("stats_input")
  enrichmentStatsS <- enrichmentStatsServer(
    "data", mapAccessionToGeneS, statsParamsS)
  drawVolcanoS <- drawVolcanoServer(
    "volcano_plot", enrichmentStatsS, sigColorS, insigColorS)
  exampleBtn <- getExampleServer("single_file", dataPathS)
  ggpairS <- plotGGpairServer("plot_ggpair", dataFrameS, extractColsS)
  # use observe event to update tab items:
  # https://stackoverflow.com/questions/51708815/accessing-parent-namespace-inside-a-shiny-module
  observeEvent(exampleBtn(), {
    updateTabItems(session, "sidebarmenu", 'dashboard')
  })
  
  # basic plot Box
  basicPlotParamServer("basic_plot_inputs", sigColorS, insigColorS)
  
  
  output$multi_FDR_colorbar <- renderPlot({
    validate(
      need(a_file_pulldown_r()  != '', "")
    )
    a_multi_vp_colorbar()
  })
  
  output$Multi_VolcanoPlot <- plotly::renderPlotly({
    #validate(need(a_file_pulldown_r()  != '', "Upload file"))
    req(a_pulldown_significant)
    a_integrated_plot()
  })
  
  output$a_file_display_table_ui <- DT::renderDataTable({
    req(a_in_pulldown())
    DT::datatable(a_in_pulldown()$data)
  })
  
  
  
  #####################
  # Multiple file tab #
  #####################

  ## handle file upload
  
  b_file_1_datapath <- reactiveVal(value = NULL)
  b_file_2_datapath <- reactiveVal(value = NULL)
  b_file_3_datapath <- reactiveVal(value = NULL)
  
  output$b_file_1_ui <- renderUI({
    fileInput('b_file_1', 'Upload file 1', accept = files_accepted)
  })
  
  output$b_file_2_ui <- renderUI({
    fileInput('b_file_2', 'Upload file 2', accept = files_accepted)
  })
  
  output$b_file_3_ui <- renderUI({
    fileInput('b_file_3', 'Upload file 3', accept = files_accepted)
  })
  
  # store paths to the data
  b_file_1_setpath <- observeEvent(input$b_file_1,{b_file_1_datapath(input$b_file_1$datapath)})
  b_file_2_setpath <- observeEvent(input$b_file_2,{b_file_2_datapath(input$b_file_2$datapath)})
  b_file_3_setpath <- observeEvent(input$b_file_3,{b_file_3_datapath(input$b_file_3$datapath)})
  
  # example files
  observeEvent(input$a_get_example_multiple_files,{
    updateTabItems(session, "sidebarmenu", 'widgets')
    b_file_1_datapath(example_file)
    b_file_2_datapath(example_file2)
    b_file_3_datapath(example_file3)
  })
  
  
 ## handle file parsing, i.e. ensure that the files are correctly mapped
 ## and contain columns requried for further analysis.
  # TODO add option to toggle two_sample_mod_ttest
 b_file_1_parsed <- reactive({
   if (!is.null(b_file_1_datapath())){
     return(parse_uploaded_file(b_file_1_datapath(), input$b_file_1_select_mod_ttest))
   } else return(NULL)
 })
  
 b_file_2_parsed <- reactive({
   if (!is.null(b_file_2_datapath())){
   return(parse_uploaded_file(b_file_2_datapath(), input$b_file_2_select_mod_ttest))
   } else return(NULL)
 })
  
 b_file_3_parsed <- reactive({
   if (!is.null(b_file_3_datapath())){
   return(parse_uploaded_file(b_file_3_datapath(), input$b_file_3_select_mod_ttest))
   } else return(NULL)
 })
 
 ## track whether input contains replicate data
 
 b_file_1_rep_bool <- reactive({
   req(b_file_1_parsed())
   check = read_input(b_file_1_datapath())$format$check
   return(check$gene_rep | check$accession_rep)
 })
 
 b_file_2_rep_bool <- reactive({
   req(b_file_2_parsed())
   check = read_input(b_file_2_datapath())$format$check
   return(check$gene_rep | check$accession_rep)
 })
 
 b_file_3_rep_bool <- reactive({
   req(b_file_3_parsed())
   check = read_input(b_file_3_datapath())$format$check
   return(check$gene_rep | check$accession_rep)
 })
 
 #
 output$b_GOI_search <- renderUI({
   textInput("b_goi_search_rep", "Search HGNC symbol")
 })
 
 output$b_GOI_search_alpha <- renderUI({
   shinyjs::hidden(sliderInput('b_goi_search_rep_alpha', 'Adjust alpha', min = 0, max = 1, value = 0.8, step = 0.05))
 })
 
 # needed for searching gene
 b_search_gene <- reactive({
   gene_in <- input$b_goi_search_rep
   if (gene_in == '') return(NULL)
   else return(toupper(gene_in))
 })
 
 ## get the available replicate in each file
 
 # Search for replicates in data
 # DONE changed this to allow for selection of sample-sample scatter plot as
 # well
 b_file_1_available_replicates <- reactive({
   validate(need(b_file_1_rep_bool(), ""))
   d <- b_file_1_parsed()
   return(enumerate_replicate_combinations(d))
 })
 
 # render select scatter plot
 output$b_file_1_select_scatterplot_ui <- renderUI({
   req(b_file_1_available_replicates())
   
   rep_input = b_file_1_available_replicates()
   reps_verbatim = gsub('rep','replicate ', rep_input)
   reps_verbatim = gsub('\\.', ' and ', reps_verbatim)
   reps = lapply(rep_input, function(x){x})
   names(reps) = reps_verbatim
   selectInput('b_file_1_select_scatterplot',
               'Replicates to compare in scatter plot', 
               choices = reps)
 })
 
 # Search for replicates in data
 b_file_2_available_replicates <- reactive({
   validate(need(b_file_2_rep_bool(), ""))
   d <- b_file_2_parsed()
   return(enumerate_replicate_combinations(d))
 })
 
 # render select scatter plot
 output$b_file_2_select_scatterplot_ui <- renderUI({
   req(b_file_2_available_replicates())
   rep_input = b_file_2_available_replicates()
   reps_verbatim = gsub('rep','replicate ', rep_input)
   reps_verbatim = gsub('\\.', ' and ', reps_verbatim)
   reps = lapply(rep_input, function(x){x})
   names(reps) = reps_verbatim
   selectInput('b_file_2_select_scatterplot',
               'Replicates to compare in scatter plot', 
               choices = reps)
 })
 
 # Search for replicates in data
 b_file_3_available_replicates <- reactive({
   validate(need(b_file_3_rep_bool(), ""))
   d <- b_file_3_parsed()
   return(enumerate_replicate_combinations(d))
 })
 
 # render select scatter plot
 output$b_file_3_select_scatterplot_ui <- renderUI({
   req(b_file_3_available_replicates())
   
   rep_input = b_file_3_available_replicates()
   reps_verbatim = gsub('rep','replicate ', rep_input)
   reps_verbatim = gsub('\\.', ' and ', reps_verbatim)
   reps = lapply(rep_input, function(x){x})
   names(reps) = reps_verbatim
   selectInput('b_file_3_select_scatterplot',
               'Replicates to compare in scatter plot', 
               choices = reps)
 })
 
 # lists of data for basic summary
 b_file_1_summary <- reactive({
   
   req(b_file_1_sp_gg_all(), b_file_1_monitor_thresholds(), b_file_1_significant())
   reps = lapply(b_file_1_sp_gg_all(), function(x) x$correlation)
   return(genoppi:::get_replicate_summary_text(b_file_1_monitor_thresholds(), b_file_1_significant(), reps))
   
 })
 
 b_file_2_summary <- reactive({
   
   req(b_file_2_sp_gg_all(), b_file_2_monitor_thresholds(), b_file_2_significant())
   reps = lapply(b_file_2_sp_gg_all(), function(x) x$correlation)
   return(genoppi:::get_replicate_summary_text(b_file_2_monitor_thresholds(), b_file_2_significant(), reps))
   
 })
 
 
 b_file_3_summary <- reactive({
   
   req(b_file_3_sp_gg_all(), b_file_3_monitor_thresholds(), b_file_3_significant())
   reps = lapply(b_file_3_sp_gg_all(), function(x) x$correlation)
   return(genoppi:::get_replicate_summary_text(b_file_3_monitor_thresholds(), b_file_3_significant(), reps))
   
 })
 
 # summary text
 
 output$b_file_1_summary_text_ui <- renderText({
   b_file_1_summary()$outtext
 })
 
 output$b_file_2_summary_text_ui <- renderText({
   b_file_2_summary()$outtext
 })
 
 output$b_file_3_summary_text_ui <- renderText({
   b_file_3_summary()$outtext
 })
 
 # summary table
 
 output$b_file_1_summary_table_ui <- renderTable({
   b_file_1_summary()$outtable
 })
 
 output$b_file_2_summary_table_ui <- renderTable({
   b_file_2_summary()$outtable
 })
 
 output$b_file_3_summary_table_ui <- renderTable({
   b_file_3_summary()$outtable
 })
 
 
 # get significance thresholds for each file
 # file 1
 output$b_file_1_logfc_direction_ui <- renderUI({
   radioButtons("b_file_1_logfc_direction", HTML("log<sub>2</sub>FC direction"),
                c("Neg" = "negative", "Both" = "both","Pos" = "positive"),
                selected = 'positive',
                inline = T)
 })
 
 output$b_file_1_significance_type_ui <- renderUI({
   radioButtons("b_file_1_significance_type", "Significance metric",
                choiceNames = list("FDR", HTML("<i>P</i>-value")),
                choiceValues = list("fdr",'pvalue'),
                inline = T)
 })
 
 output$b_file_1_FDR_thresh <- renderUI({
   sliderInput("b_file_1_fdr_thresh", "FDR threshold",
               min = 0, max = 1, value = 0.1, step = 0.01)
 })
 
 output$b_file_1_PVal_thresh <- renderUI({
   sliderInput("b_file_1_pval_thresh", HTML("<i>P</i>-value threshold"),
               min = 0, max = 1, value = 0.05, step = 0.001)
 })
 
 #TODO
 select_mod_ttest_server("b_file_1_select_mod_ttest")
 select_mod_ttest_server("b_file_2_select_mod_ttest")
 select_mod_ttest_server("b_file_3_select_mod_ttest")
 
 
 output$b_file_1_logFC_thresh <- renderUI({
   req(b_file_1_parsed(), input$b_file_1_logfc_direction)
   if(!is.null(b_file_1_parsed())){
     limit <- calc_logfc_limit(b_file_1_parsed(), input$b_file_1_logfc_direction)
     sliderInput("b_file_1_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                 min = 0, max = limit, value = 0, step = 0.1)
   } else
     sliderInput("b_file_1_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                 min = 0, max = 1, value = 0, step = 0.1)
 })  
 
 # file 2
 output$b_file_2_logfc_direction_ui <- renderUI({
   radioButtons("b_file_2_logfc_direction", HTML("log<sub>2</sub>FC direction"),
                c("Neg" = "negative", "Both" = "both","Pos" = "positive"),
                selected = 'positive',
                inline = T)
 })
 
 output$b_file_2_significance_type_ui <- renderUI({
   radioButtons("b_file_2_significance_type", "Significance metric",
                choiceNames = list("FDR", HTML("<i>P</i>-value")),
                choiceValues = list("fdr",'pvalue'),
                inline = T)
 })
 
 output$b_file_2_FDR_thresh <- renderUI({
   sliderInput("b_file_2_fdr_thresh", "FDR threshold",
               min = 0, max = 1, value = 0.1, step = 0.01)
 })
 
 output$b_file_2_PVal_thresh <- renderUI({
   sliderInput("b_file_2_pval_thresh", HTML("<i>P</i>-value threshold"),
               min = 0, max = 1, value = 0.05, step = 0.001)
 })
 
 output$b_file_2_logFC_thresh <- renderUI({
   req(b_file_2_parsed(), input$b_file_2_logfc_direction)
   if(!is.null(b_file_2_parsed())){
     limit <- calc_logfc_limit(b_file_2_parsed(), input$b_file_2_logfc_direction)
     sliderInput("b_file_2_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                 min = 0, max = limit, value = 0, step = 0.1)
   } else
     sliderInput("b_file_2_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                 min = 0, max = 1, value = 0, step = 0.1)
 })  
 
 # file 3
 output$b_file_3_logfc_direction_ui <- renderUI({
   radioButtons("b_file_3_logfc_direction", HTML("log<sub>2</sub>FC direction"),
                c("Neg" = "negative", "Both" = "both","Pos" = "positive"),
                selected = 'positive',
                inline = T)
 })
 
 output$b_file_3_significance_type_ui <- renderUI({
   radioButtons("b_file_3_significance_type", "Significance metric",
                choiceNames = list("FDR", HTML("<i>P</i>-value")),
                choiceValues = list("fdr",'pvalue'),
                inline = T)
 })
 
 output$b_file_3_FDR_thresh <- renderUI({
   sliderInput("b_file_3_fdr_thresh", "FDR threshold",
               min = 0, max = 1, value = 0.1, step = 0.01)
 })
 
 output$b_file_3_PVal_thresh <- renderUI({
   sliderInput("b_file_3_pval_thresh", HTML("<i>P</i>-value threshold"),
               min = 0, max = 1, value = 0.05, step = 0.001)
 })
 
 output$b_file_3_logFC_thresh <- renderUI({
   req(b_file_3_parsed(), input$b_file_3_logfc_direction)
   if(!is.null(b_file_3_parsed())){
     limit <- calc_logfc_limit(b_file_3_parsed(), input$b_file_3_logfc_direction)
     sliderInput("b_file_3_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                 min = 0, max = limit, value = 0, step = 0.1)
   } else
     sliderInput("b_file_3_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                 min = 0, max = 1, value = 0, step = 0.1)
 })  
 
 ##
 observeEvent(input$b_file_1_significance_type,{
   if (input$b_file_1_significance_type == 'fdr'){
     shinyjs::hide("b_file_1_pval_thresh")
     shinyjs::show("b_file_1_fdr_thresh")
   } else {
     shinyjs::show("b_file_1_pval_thresh")
     shinyjs::hide("b_file_1_fdr_thresh")
   }
 })
 observeEvent(input$b_file_2_significance_type,{
   if (input$b_file_2_significance_type == 'fdr'){
     shinyjs::hide("b_file_2_pval_thresh")
     shinyjs::show("b_file_2_fdr_thresh")
   } else {
     shinyjs::show("b_file_2_pval_thresh")
     shinyjs::hide("b_file_2_fdr_thresh")
   }
 })
 observeEvent(input$b_file_3_significance_type,{
   if (input$b_file_3_significance_type == 'fdr'){
     shinyjs::hide("b_file_3_pval_thresh")
     shinyjs::show("b_file_3_fdr_thresh")
   } else {
     shinyjs::show("b_file_3_pval_thresh")
     shinyjs::hide("b_file_3_fdr_thresh")
   }
 })
 
 ## track thresholds 

 b_file_1_monitor_thresholds <- reactive({
   
   html_translate_significance_thresholds(fc = input$b_file_1_logFC_thresh,
                                         fc_dir = input$b_file_1_logfc_direction, 
                                         sig_type = input$b_file_1_significance_type, 
                                         fdr_thresh = input$b_file_1_fdr_thresh, 
                                         pval_thresh = input$b_file_1_pval_thresh)
   
 })
 
 
 b_file_2_monitor_thresholds <- reactive({
   
   html_translate_significance_thresholds(fc = input$b_file_2_logFC_thresh,
                                         fc_dir = input$b_file_2_logfc_direction, 
                                         sig_type = input$b_file_2_significance_type, 
                                         fdr_thresh = input$b_file_2_fdr_thresh, 
                                         pval_thresh = input$b_file_2_pval_thresh)
   
 })
 b_file_3_monitor_thresholds <- reactive({
   
   html_translate_significance_thresholds(fc = input$b_file_3_logFC_thresh,
                                         fc_dir = input$b_file_3_logfc_direction, 
                                         sig_type = input$b_file_3_significance_type, 
                                         fdr_thresh = input$b_file_3_fdr_thresh, 
                                         pval_thresh = input$b_file_3_pval_thresh)
   
 })
 

 ## Handle significance for each file
 b_file_1_significant <- reactive({
   req(input$b_file_1_significance_type)
   if (!is.null(b_file_1_parsed())){
     d = b_file_1_parsed()
     if (input$b_file_1_significance_type == 'fdr'){
       d1 = id_enriched_proteins(d, fdr_cutoff = input$b_file_1_fdr_thresh, logfc_dir = input$b_file_1_logfc_direction, 
                                 logfc_cutoff = input$b_file_1_logFC_thresh)
     } else {
       d1 = id_enriched_proteins(d, fdr_cutoff = NULL, p_cutoff = input$b_file_1_pval_thresh, logfc_dir = input$b_file_1_logfc_direction, 
                                 logfc_cutoff = input$b_file_1_logFC_thresh)
     }
     return(d1)
   } else (return(NULL))
 })
 
 b_file_2_significant <- reactive({
   req(input$b_file_2_significance_type)
   if (!is.null(b_file_2_parsed())){
     d = b_file_2_parsed()
     if (input$b_file_2_significance_type == 'fdr'){
       d1 = id_enriched_proteins(d, fdr_cutoff = input$b_file_2_fdr_thresh, logfc_dir = input$b_file_2_logfc_direction, 
                                 logfc_cutoff = input$b_file_2_logFC_thresh)
     } else {
       d1 = id_enriched_proteins(d, fdr_cutoff = NULL, p_cutoff = input$b_file_2_pval_thresh, logfc_dir = input$b_file_2_logfc_direction, 
                                 logfc_cutoff = input$b_file_2_logFC_thresh)
     }
     return(d1)
   } else (return(NULL))
 })
 
 b_file_3_significant <- reactive({
   req(input$b_file_3_significance_type)
   if (!is.null(b_file_3_parsed())){
     d = b_file_3_parsed()
     if (input$b_file_3_significance_type == 'fdr'){
       d1 = id_enriched_proteins(d, fdr_cutoff = input$b_file_3_fdr_thresh, logfc_dir = input$b_file_3_logfc_direction, 
                                 logfc_cutoff = input$b_file_3_logFC_thresh)
     } else {
       d1 = id_enriched_proteins(d, fdr_cutoff = NULL, p_cutoff = input$b_file_3_pval_thresh, logfc_dir = input$b_file_3_logfc_direction, 
                                 logfc_cutoff = input$b_file_3_logFC_thresh)
     }
     return(d1)
   } else (return(NULL))
 })
 
 # get overlap of each file
 b_overlap <- reactive({
   
   inputs = list(f1=b_file_1_significant(), f2=b_file_2_significant(), f3=b_file_3_significant())
   genes = lapply(inputs, function(x) if (!is.null(x)) return(as.character(x$gene[x$significant])))
   genes_clean = null_omit(genes)
   return(genes_clean)
   
 })
 
 ### determine overlap
 b_mapping <- reactive({
   genes = b_overlap()
   req(genes)
   if (length(genes) == 3){
     venn = list(
       f1 = genes$f1[genes$f1 %nin% c(genes$f3, genes$f2)], 
       f2 = genes$f2[genes$f2 %nin% c(genes$f1, genes$f3)], 
       f3 = genes$f3[genes$f3 %nin% c(genes$f1, genes$f2)],
       f12 = intersect(genes$f1, genes$f2)[intersect(genes$f1, genes$f2) %nin% Reduce(intersect, genes)], 
       f13 = intersect(genes$f1, genes$f3)[intersect(genes$f1, genes$f3) %nin% Reduce(intersect, genes)], 
       f23 = intersect(genes$f2, genes$f3)[intersect(genes$f2, genes$f3) %nin% Reduce(intersect, genes)],
       f123 = Reduce(intersect, genes)
     )
   } else if (length(genes) < 3){
     venn = list(
       f1 = genes$f1[genes$f1 %nin% c(genes$f3, genes$f2)], 
       f2 = genes$f2[genes$f2 %nin% c(genes$f1, genes$f3)], 
       f3 = genes$f3[genes$f3 %nin% c(genes$f1, genes$f2)],
       f12 = intersect(genes$f1, genes$f2),
       f13 = intersect(genes$f1, genes$f3),
       f23 = intersect(genes$f2, genes$f3)
     )
   }

   # venn colors
   colors = list(
     f1 = 'red',
     f2 = 'yellow',
     f3 = 'blue',
     f12 = 'orange',
     f13 = 'purple',
     f23 = 'green',   
     f123 = 'white'
   )
   
   # build data 
   venn_colored = lapply(1:length(venn), function(i){
     x = venn[[i]]
     color = colors[[i]]
     dataset = names(venn)[i]
     if (length(x) > 0) return(data.frame(gene=x, col_significant=color, dataset = dataset))
   })
   
   names(venn_colored) = names(venn)
   venn_colored = null_omit(venn_colored)
   
   return(venn_colored)

 })
 
 # venn mapping for file 1
 b_file_1_mapping <- reactive({
   
   d = b_file_1_significant()
   mapping = b_mapping()
   mapping = mapping[grepl('1',names(mapping))]
   mapping = lapply(mapping, function(x) to_overlay_data(x[x$gene %in% d$gene,]))
   mapping = do.call(rbind, mapping)
   mapping$label = F
   return(mapping)
   
 })
 
 b_file_1_vp_gg <- reactive({
   
   req(b_file_1_significant())
   d <- b_file_1_significant()
   p <- plot_volcano_basic(d)
   p <- plot_overlay(p, list(overlay=b_file_1_mapping()))
   return(p)
   
 })
 
 b_file_1_sp_gg_all <- reactive({
   
   req(b_file_1_significant())
   d <- b_file_1_significant()
   p <- plot_scatter_basic_all(d)
   return(p)
   
 })
 
 b_file_1_sp_gg <- reactive({
   
   req(b_file_1_sp_gg_all(), input$b_file_1_select_scatterplot)
   p <- b_file_1_sp_gg_all()[[input$b_file_1_select_scatterplot]]$ggplot
   p$r <- p$correlation
   p <- plot_overlay(p, list(overlay=b_file_1_mapping()))
   return(p)
   
 })
 
 b_file_1_vp <- reactive({
   req(input$b_file_1_pval_thresh, input$b_file_1_logFC_thresh, input$b_file_1_logfc_direction, input$b_file_1_significance_type)
   p <- b_file_1_vp_gg()
   p$overlay = p$overlay[order(p$overlay$dataset),]
   p$overlay$legend_order = 1:nrow(p$overlay)
   p <- make_interactive(p, legend = T)
   if (input$b_goi_search_rep != '') p <- add_plotly_markers_search(p, b_search_gene(), alpha = input$b_goi_search_rep_alpha)
   p <- genoppi::add_plotly_threshold_lines (p, line_pvalue = input$b_file_1_pval_thresh, line_logfc = input$b_file_1_logFC_thresh, logfc_direction = input$b_file_1_logfc_direction, sig_type = input$b_file_1_significance_type)
   p <- add_plotly_layout_volcano(p, NULL, NULL, orientation = 'h', legend.y = 10)
   return(p)
   
 })
 
 b_file_1_sp <- reactive({
   
   p <- b_file_1_sp_gg()
   p <- make_interactive(p, legend = F)
   if (input$b_goi_search_rep != '') p <- add_plotly_markers_search(p, b_search_gene(), alpha = input$b_goi_search_rep_alpha)
   p <- add_plotly_layout_scatter(p, NULL, NULL, orientation = 'h')
   return(p)
   
 })
  
 output$b_file_1_volcano = plotly::renderPlotly({
   req(b_file_1_vp())
   b_file_1_vp()
 })
 
 output$b_file_1_scatter = plotly::renderPlotly({
   req(b_file_1_sp())
   b_file_1_sp()
 })
 
 
 # venn mapping for file 1
 b_file_2_mapping <- reactive({
   
   d = b_file_2_significant()
   mapping = b_mapping()
   mapping = mapping[grepl('2',names(mapping))]
   mapping = lapply(mapping, function(x) to_overlay_data(x[x$gene %in% d$gene,]))
   mapping = do.call(rbind, mapping)
   mapping$label = F
   return(mapping)
   
 })
 
 b_file_2_vp_gg <- reactive({
   
   req(b_file_2_significant())
   d <- b_file_2_significant()
   p <- plot_volcano_basic(d)
   p <- plot_overlay(p, list(overlay=b_file_2_mapping()))
   return(p)
   
 })
 
 b_file_2_sp_gg_all <- reactive({
   
   req(b_file_2_significant())
   d <- b_file_2_significant()
   p <- plot_scatter_basic_all(d)
   return(p)
   
 })
 
 b_file_2_sp_gg <- reactive({
   
   req(b_file_2_sp_gg_all(), input$b_file_2_select_scatterplot)
   p <- b_file_2_sp_gg_all()[[input$b_file_2_select_scatterplot]]$ggplot
   p$r <- p$correlation
   p <- plot_overlay(p, list(overlay=b_file_2_mapping()))
   return(p)
   
 })
 
 b_file_2_vp <- reactive({
   req(input$b_file_2_pval_thresh, input$b_file_2_logFC_thresh, input$b_file_2_logfc_direction, input$b_file_2_significance_type)
   p <- b_file_2_vp_gg()
   p$overlay = p$overlay[order(p$overlay$dataset),]
   p$overlay$legend_order = 1:nrow(p$overlay)
   p <- make_interactive(p, legend = T)
   if (input$b_goi_search_rep != '') p <- add_plotly_markers_search(p, b_search_gene(), alpha = input$b_goi_search_rep_alpha)
   p <- genoppi::add_plotly_threshold_lines (p, line_pvalue = input$b_file_2_pval_thresh, line_logfc = input$b_file_2_logFC_thresh, logfc_direction = input$b_file_2_logfc_direction, sig_type = input$b_file_2_significance_type)
   p <- add_plotly_layout_volcano(p, NULL, NULL, orientation = 'h', legend.y = 10)
   return(p)
   
 })
 
 b_file_2_sp <- reactive({
   
   p <- b_file_2_sp_gg()
   p <- make_interactive(p, legend = F)
   if (input$b_goi_search_rep != '') p <- add_plotly_markers_search(p, b_search_gene(), alpha = input$b_goi_search_rep_alpha)
   p <- add_plotly_layout_scatter(p, NULL, NULL, orientation = 'h')
   
   return(p)
   
 })
 
 output$b_file_2_volcano = plotly::renderPlotly({
   req(b_file_2_vp())
   b_file_2_vp()
 })
 
 output$b_file_2_scatter = plotly::renderPlotly({
   req(b_file_2_sp())
   b_file_2_sp()
 })
 
 b_file_3_mapping <- reactive({
   
   d = b_file_3_significant()
   mapping = b_mapping()
   mapping = mapping[grepl('3',names(mapping))]
   mapping = lapply(mapping, function(x) to_overlay_data(x[x$gene %in% d$gene,]))
   mapping = do.call(rbind, mapping)
   mapping$label = F
   return(mapping)
   
 }) 
 
 b_file_3_vp_gg <- reactive({
   
   req(b_file_3_significant())
   d <- b_file_3_significant()
   p <- plot_volcano_basic(d)
   p <- plot_overlay(p, list(overlay=b_file_3_mapping()))
   return(p)
   
 })
 
 b_file_3_sp_gg_all <- reactive({
   
   req(b_file_3_significant())
   d <- b_file_3_significant()
   p <- plot_scatter_basic_all(d)
   return(p)
   
 })
 
 b_file_3_sp_gg <- reactive({
   
   req(b_file_3_sp_gg_all(), input$b_file_3_select_scatterplot)
   p <- b_file_3_sp_gg_all()[[input$b_file_3_select_scatterplot]]$ggplot
   p$r <- p$correlation
   p <- plot_overlay(p, list(overlay=b_file_3_mapping()))
   return(p)
   
 })
 
 b_file_3_vp <- reactive({
   req(input$b_file_3_pval_thresh, input$b_file_3_logFC_thresh, input$b_file_3_logfc_direction, input$b_file_3_significance_type)
   p <- b_file_3_vp_gg()
   p$overlay = p$overlay[order(p$overlay$dataset),]
   p$overlay$legend_order = 1:nrow(p$overlay)
   p <- make_interactive(p, legend = T)
   if (input$b_goi_search_rep != '') p <- add_plotly_markers_search(p, b_search_gene(), alpha = input$b_goi_search_rep_alpha)
   p <- genoppi::add_plotly_threshold_lines (p, line_pvalue = input$b_file_3_pval_thresh, line_logfc = input$b_file_3_logFC_thresh, logfc_direction = input$b_file_3_logfc_direction, sig_type = input$b_file_3_significance_type)
   p <- add_plotly_layout_volcano(p, NULL, NULL, orientation = 'h', legend.y = 10)
   return(p)
   
 })
 
 b_file_3_sp <- reactive({
   
   p <- b_file_3_sp_gg()
   p <- make_interactive(p, legend = F)
   if (input$b_goi_search_rep != '') p <- add_plotly_markers_search(p, b_search_gene(), alpha = input$b_goi_search_rep_alpha)
   p <- add_plotly_layout_scatter(p, NULL, NULL, orientation = 'h')
   return(p)
   
 })
 
 output$b_file_3_volcano = plotly::renderPlotly({
   req(b_file_3_vp())
   b_file_3_vp()
 })
 
 output$b_file_3_scatter = plotly::renderPlotly({
   req(b_file_3_sp())
   b_file_3_sp()
 })
 
 # draw venn diagram for gnomAD
 output$b_file_comparison_venn_ui <- renderPlot({
   req(b_overlap())
   diagram = b_overlap()
   if (length(diagram) > 1) {
     color_dict = list(f1='red',f2='yellow',f3='blue')
     colors = unlist(lapply(names(diagram), function(x) color_dict[[x]]))
     names(diagram) = gsub('f', 'file ', names(diagram))
     v = draw_genoppi_venn(diagram, color = colors, main = '')
     grid::grid.newpage()
     grid::pushViewport(grid::viewport(width=unit(0.9, "npc"), height = unit(0.9, "npc")))
     grid::grid.draw(v)
   } else return(NULL)

 })
 
 output$b_file_comparison_venn_explanations_ui <- renderUI({
   
   req(b_mapping())
   input = unique(names(b_mapping()))
   
   text = list(
     f1 = paste(bold('f1:'), 'significant proteins unique to file1 (red)'),
     f2 = paste(bold('f2:'), 'significant proteins unique to file2 (yellow)'),
     f3 = paste(bold('f3:'), 'significant proteins unique to file3 (blue)'),
     f12 = paste(bold('f12:'), 'significant proteins identified in file1 and file2 (orange)'),
     f13 = paste(bold('f13:'), 'significant proteins identified in file1 and file3 (purple)'),
     f23 = paste(bold('f23:'), 'significant proteins identified in file2 and file3 (green)'),
     f123 = paste(bold('f123:'), 'significant proteins identified in file1, file2, and file3 (white)')
   )
   
  return(HTML(paste(text[names(text) %in% input], collapse = "<br/>")))
   
 })
 
 # text for venn diagram
 b_file_comparison_venn_verbatim <- reactive({
   req(b_overlap())
   overlap = b_overlap()
   
   # get thresholds and combine them 
   all_thresholds = list(f1 = b_file_1_monitor_thresholds(),
                         f2 = b_file_2_monitor_thresholds(),
                         f3 = b_file_3_monitor_thresholds())
   thresholds = lapply(all_thresholds[names(all_thresholds) %in% names(overlap)], 
                       function(x) paste(x$sig$sig, x$fc$sig, sep = ', '))
   
   # generate text for displaying
   result = lapply(names(overlap), function(f){
     paste0(gsub('f', 'file ', f),' = proteomic data subsetted by ', thresholds[[f]], " &#40;", bold(length(overlap[[f]])), "&#41;")
   })
   
   names(result) <- names(overlap)
   return(result)
 })
  
 output$b_file_comparison_venn_verbatim_ui <- renderUI({
   output <- b_file_comparison_venn_verbatim()
   HTML(paste(output, collapse = "<br/>"))
 })
 
 
 # select 
 output$b_file_comparison_data_table_select_ui <- renderUI({
   overlap = b_mapping()
            selectInput("b_file_comparison_data_table_select",
                        "Subset data",
                        c("All",
                          unique(as.character(names(overlap)))))
 })

 
 # make data.table for showing overlap
 b_file_comparison_data_table <- reactive({
   req(b_overlap())
   overlap = b_mapping()
   selected = input$b_file_comparison_data_table_select
   if (any(selected %nin% 'All')){
     overlap = overlap[names(overlap) %in% selected]
   }
   
   lst = lapply(names(overlap), function(x){overlap[[x]][,c('gene','dataset')]})
   df = do.call(rbind, lst)
   return(df)
 })
 
 # show table
 output$b_file_comparison_data_table_ui <- DT::renderDataTable({
   req(b_file_comparison_data_table())
   df = b_file_comparison_data_table()
   DT::datatable(df)
 })
  
 
 #------------------------------------------------------
 # download multiple files volcano and scatter plots
 
 # download basic volcano and scatter plot
 input_b_file_1_vp_gg <- function(){b_file_1_vp_gg()}
 output$b_file_1_volcano_download = downloadHandler(
   filename = paste("genoppi-multi-volcano-file1",".png", sep=""),
   content = function(file) {
     device <- function(..., width, height) {
       grDevices::png(..., width = width, height = height,
                      res = 300, units = "in")
     }
     ggsave(file, plot = theme_volcano(input_b_file_1_vp_gg()), device = device, 
            width = global.img.scatter.download.width,
            height = global.img.scatter.download.height)
   })
 
 input_b_file_1_sp_gg <- function(){b_file_1_sp_gg()}
 output$b_file_1_scatter_download = downloadHandler(
   filename = paste("genoppi-multi-scatter-file1",".png", sep=""),
   content = function(file) {
     device <- function(..., width, height) {
       grDevices::png(..., width = width, height = height,
                      res = 300, units = "in")
     }
     ggsave(file, plot = theme_scatter(input_b_file_1_sp_gg()), device = device, 
            width = global.img.scatter.download.width,
            height = global.img.scatter.download.height)
   })
 
 
 # download proteomic data
 output$b_file_1_mapping_download <- downloadHandler(
   filename = function() {
     paste("genoppi-f1-multiple-comparison",".csv", sep="")
   },
   content = function(file) {
     write.csv(b_file_1_significant(), file, row.names = F)
   }
 )
 
 
 # show/hide buttons
 observeEvent(b_file_1_significant(),{
   shinyjs::show("b_file_1_volcano_download")
   shinyjs::show("b_file_1_scatter_download")
   shinyjs::show("b_file_1_mapping_download")
 })
 
 ##
 
 # download basic volcano and scatter plot
 input_b_file_2_vp_gg <- function(){b_file_2_vp_gg()}
 output$b_file_2_volcano_download = downloadHandler(
   filename = paste("genoppi-multi-volcano-file2",".png", sep=""),
   content = function(file) {
     device <- function(..., width, height) {
       grDevices::png(..., width = width, height = height,
                      res = 300, units = "in")
     }
     ggsave(file, plot = theme_volcano(input_b_file_2_vp_gg()), device = device, 
            width = global.img.scatter.download.width,
            height = global.img.scatter.download.height)
   })
 
 input_b_file_2_sp_gg <- function(){b_file_2_sp_gg()}
 output$b_file_2_scatter_download = downloadHandler(
   filename = paste("genoppi-multi-scatter-file2",".png", sep=""),
   content = function(file) {
     device <- function(..., width, height) {
       grDevices::png(..., width = width, height = height,
                      res = 300, units = "in")
     }
     ggsave(file, plot = theme_scatter(input_b_file_2_sp_gg()), device = device, 
            width = global.img.scatter.download.width,
            height = global.img.scatter.download.height)
   })
 
 # download proteomic data
 output$b_file_2_mapping_download <- downloadHandler(
   filename = function() {
     paste("genoppi-f2-multiple-comparison",".csv", sep="")
   },
   content = function(file) {
     write.csv(b_file_2_significant(), file, row.names = F)
   }
 )
 
 # show/hide buttons
 observeEvent(b_file_2_significant(),{
   shinyjs::show("b_file_2_volcano_download")
   shinyjs::show("b_file_2_scatter_download")
   shinyjs::show("b_file_2_mapping_download")
 })
 
 
 # download basic volcano and scatter plot
 input_b_file_3_vp_gg <- function(){b_file_3_vp_gg()}
 output$b_file_3_volcano_download = downloadHandler(
   filename = paste("genoppi-multi-volcano-file3",".png", sep=""),
   content = function(file) {
     device <- function(..., width, height) {
       grDevices::png(..., width = width, height = height,
                      res = 300, units = "in")
     }
     ggsave(file, plot = theme_volcano(input_b_file_3_vp_gg()), device = device, 
            width = global.img.scatter.download.width,
            height = global.img.scatter.download.height)
   })
 
 input_b_file_3_sp_gg <- function(){b_file_3_sp_gg()}
 output$b_file_3_scatter_download = downloadHandler(
   filename = paste("genoppi-multi-scatter-file3",".png", sep=""),
   content = function(file) {
     device <- function(..., width, height) {
       grDevices::png(..., width = width, height = height,
                      res = 300, units = "in")
     }
     ggsave(file, plot = theme_scatter(input_b_file_3_sp_gg()), device = device, 
            width = global.img.scatter.download.width,
            height = global.img.scatter.download.height)
   })
 
 # download mapping
 output$b_file_3_mapping_download <- downloadHandler(
   filename = function() {
     paste("genoppi-f3-multiple-comparison",".csv", sep="")
   },
   content = function(file) {
     write.csv(b_file_3_significant(), file, row.names = F)
   }
 )
 
 # show/hide buttons
 observeEvent(b_file_3_significant(),{
   shinyjs::show("b_file_3_volcano_download")
   shinyjs::show("b_file_3_scatter_download")
   shinyjs::show("b_file_3_mapping_download")
 })
 
 # download protein family mapping? gene annotations
 output$b_file_comparison_data_table_download_ui <- downloadHandler(
   filename = function() {
     paste("genoppi-multi-venn-mapping",".csv", sep="")
   },
   content = function(file) {
    df = b_file_comparison_data_table()
    write.csv(df, file, row.names = F)
   }
 )

 

 

 
 
  
  ##### GENERAL #####
  #documentation
 
  output$info_inweb_ui <- renderUI({actionLink('info_inweb', ' ', icon = icon('question-circle'))})
  observeEvent(input$info_inweb,{
    #text = paste(readLines('documentation/inweb_table.info'))
    
    text = paste(readLines(find_docs('inweb_table.info')), '<br> <br>', 
                 readLines(find_docs('irefindex_table.info')), '<br> <br>', 
                 readLines(find_docs('bioplex_table.info')))
    showModal(modalDialog(HTML(text), easyClose = T, footer=genoppi.ver, title = 'InWeb'))
  })
  
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
    
    #return(tags$iframe(src = "welcome_guide_200509.pdf",
    #                   style="width:100%;",  #frameborder="0"
    #                   height = "3100px"))
  }
  
  
  
  output$how_to_guide <- renderUI({
    # Uncomment to get welcome guide
    # getPage_guide()
  })
  
  
  # output$documentation <- renderUI({
  #   return(includeHTML("documentation/doc_0316.html")
  #   )
  # })
  
})



