
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
  output$a_file <- renderUI({
    fileInput('a_file_pulldown_r', 'Upload user input file',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv')
    )
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
  
  output$PVal_thresh <- renderUI({
    #validate(need(input$a_significance_type == 'pvalue', ''))
    sliderInput("a_pval_thresh", HTML("<i>P</i>-value threshold"),
                min = 0, max = 1, value = 0.05, step = 0.001)
  })
  
  
  
  # based on a_pulldown(), create slider for logFC
  output$logFC_thresh <- renderUI({
    if(!is.null(input$a_file_pulldown_r)){
      df <- a_pulldown()
      if (input$a_logfc_direction == 'negative'){
        limit <- abs(min(df$logFC))
        limit <- round(limit+0.5, 1)
      } else if (input$a_logfc_direction == 'positive'){
        limit <- max(df$logFC)
        limit <- round(limit+0.5, 1)
      } else {
        limit <- max(max(df$logFC), abs(min(df$logFC)))
        limit <- round(limit+0.5, 1)
      }
      sliderInput("a_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                  min = 0, max = limit, value = 0, step = 0.1)
    }else
      sliderInput("a_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
                  min = 0, max = 1, value = 0, step = 0.1)
  })  
  
  # based on a_pulldown(), create slider for logFC
  output$logFC_thresh_old <- renderUI({
    if(!is.null(input$a_file_pulldown_r)){
      df <- a_pulldown()
      min_logFC <- min(df$logFC)
      min_logFC <- round(min_logFC-0.5, 1)
      max_logFC <- max(df$logFC)
      max_logFC <- round(max_logFC+0.5, 1)
      sliderInput("a_logFC_thresh", "logFC threshold",
                  min = min_logFC, max = max_logFC, value = c(0, max_logFC), step = 0.1)
    }else
      sliderInput("a_logFC_thresh", "logFC threshold",
                  min = -1, max = 1, value = c(0, 1), step = 0.1)
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

  #----------------------------------------------------------
  # sliders for selecting color, symbols and labels of plots
  
  # basic plot
  output$a_color_theme_indv_sig <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c('Colors for ',monitor_significance_thresholds()$sig, 'and', monitor_logfc_threshold()$sig)))
    colourpicker::colourInput('a_color_indv_sig', label, value = '#41AB5D', showColour = 'both', 
                  palette = c( "limited"), allowedCols = allowed_colors)
  })
    
  # basic plot
  output$a_color_theme_indv_insig <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c('Colors for ',monitor_significance_thresholds()$insig, 'and', monitor_logfc_threshold()$insig)))
    colourpicker::colourInput('a_color_indv_insig', label, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  
  # intgrated plot, snp
  output$a_color_snp_sig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', '))
    colourpicker::colourInput('a_color_snp_sig', label, value = 'blue', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, snp
  output$a_color_snp_insig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', '))
    colourpicker::colourInput('a_color_snp_insig', label, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # integrated plot, snp
  output$a_symbol_snp_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    selectInput('a_symbol_snp', 'Symbol', choices = allowed_plotly_symbols)
  })
  # integrated plot, snp
  output$a_label_snp_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    checkboxInput("a_label_snp", label = "Toggle labels", value = TRUE)
  })
  
  # integrated plot, snp
  output$a_overlay_snp_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    checkboxInput("a_overlay_snp", label = "Toggle overlay", value = TRUE)
  })
  
  # integrated plot, snp,
  output$a_reset_snp_ui <- renderUI({
    actionButton('a_reset_snp','clear')
  })
  
  
  # intgrated plot, genes upload
  output$a_color_genes_upload_sig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', '))
    colourpicker::colourInput('a_color_genes_upload_sig', label, value = '#A52A2A', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, genes upload
  output$a_color_genes_upload_insig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', '))
    colourpicker::colourInput('a_color_genes_upload_insig', label, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  
  # integrated plot, genes uplaod
  output$a_symbol_genes_upload_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    selectInput('a_symbol_genes_upload', 'Symbol', choices = allowed_plotly_symbols)
  })
  
  # integrated plot, genes upload
  output$a_label_genes_upload_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    checkboxInput("a_label_genes_upload", label = "Toggle labels", value = TRUE)
  })
  
  # integrated plot, genes upload
  output$a_overlay_genes_upload_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    checkboxInput("a_overlay_genes_upload", label = "Toggle overlay", value = TRUE)
  })
  
  # integrated plot, reset
  output$a_reset_genes_upload_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    actionButton('a_reset_genes_upload', 'Reset')
  })
  observeEvent(input$a_reset_genes_upload, {
    reset("a_file_genes_rep")
  })
  
  
  # intgrated plot, inweb
  output$a_color_inweb_sig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', '))
    colourpicker::colourInput('a_color_inweb_sig', label, value = 'yellow', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, inweb
  output$a_color_inweb_insig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', '))
    colourpicker::colourInput('a_color_inweb_insig', label, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # integrated plot, inweb
  output$a_symbol_inweb_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    selectInput('a_symbol_inweb', 'Symbol', choices = allowed_plotly_symbols)
  })
  # integrated plot, inweb
  output$a_label_inweb_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    checkboxInput("a_label_inweb", label = "Toggle labels", value = TRUE)
  })
  
  # intgrated plot, gwas catalogue
  output$a_color_gwas_cat_sig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', '))
    colourpicker::colourInput('a_color_gwas_cat_sig', label, value = 'cyan', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, gwas catalogue
  output$a_color_gwas_cat_insig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', '))
    colourpicker::colourInput('a_color_gwas_cat_insig', label, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # integrated plot, gwas catalogue
  output$a_symbol_gwas_cat_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    selectInput('a_symbol_gwas_cat', 'Symbol', choices = allowed_plotly_symbols)
  })
  # integrated plot, genes upload
  output$a_label_gwas_cat_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    checkboxInput("a_label_gwas_cat", label = "Toggle labels", value = TRUE)
  })
  
  
  # intgrated plot, gnomad
  output$a_color_gnomad_sig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig), collapse =', '))
    colourpicker::colourInput('a_color_gnomad_sig', label, value = '#FF00FF', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, gnomad
  output$a_color_gnomad_insig_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    label = HTML(paste(c(monitor_significance_thresholds()$insig, monitor_logfc_threshold()$insig), collapse =', '))
    colourpicker::colourInput('a_color_gnomad_insig', label, value = '#808080', showColour = 'both', 
                              palette = c( "limited"), allowedCols = allowed_colors)
  })
  # intgrated plot, gnomad
  output$a_symbol_gnomad_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    selectInput('a_symbol_gnomad', 'Symbol', choices = allowed_plotly_symbols)
  })
  
  # intgrated plot, gnomad
  output$a_label_gnomad_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    checkboxInput("a_label_gnomad", label = "Toggle labels", value = FALSE)
  })

  # integrated plot, gnomad 
  output$a_select_gnomad_pli_type_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    radioButtons("a_select_gnomad_pli_type", label = 'Select pLI type', 
                 choiceNames = list('None','Threshold'),
                 choiceValues = list('none','threshold'))
  })
  
  # integrated plot, gnomad slider
  output$a_slide_gnomad_pli_threshold_ui <- renderUI({
    #validate(need(input$a_file_pulldown_r != '', ""))
    validate(need(input$a_select_gnomad_pli_type == 'threshold', ""))
    sliderInput(inputId = "a_slide_gnomad_pli_threshold", label = 'Subset interactors by pLI threshold', 
                min = 0, max = 1, value = 0.5, step = 0.01)
  })
  
  
  output$a_bait_layer <- renderUI({
    textInput("a_bait_rep", value = "", "Input HGNC symbol to search for InWeb protein interactors (e.g. ZBTB7A)")
  })
  
  output$a_bait_search <- renderUI({
    textInput("a_bait_search_rep", "Input bait (HGNC symbol, e.g. BCL2)")
  })
  
  output$a_GOI_search <- renderUI({
    textInput("a_goi_search_rep", "Search HGNC symbol")
  })
  
  output$a_gwas_catalogue_ui <- renderUI({
    selectInput('a_gwas_catalogue', 'Search GWAS catalog trait(s):', unique(as.character(as.vector(gwas_table$DISEASE.TRAIT))), multiple=T, selectize=TRUE, selected = "grey")
  })
  
  # Search for replicates in data
  available_replicates <- reactive({
    req(a_pulldown())
    d <- a_pulldown()
    reps = sum(grepl('^rep[0-9]+$',colnames(d)))
    if (reps > 2){
      enum = enumerate_replicate_combinations(reps)
      enum = apply(enum, 2, function(x) paste0('rep', x))
      return(paste0(enum[,1],'.',enum[,2]))
    } else {return('rep1.rep2')}
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
  
  # render replicate summary
  output$a_replicate_summary_table_ui <- renderTable({
    #req(replicate_summary_table())
    replicate_summary_table()
  })
  
  # render select scatter plot
  output$a_select_scatterplot_ui <- renderUI({
    
    # rename reps
    
    rep_input = available_replicates()
    #if (!is.null(rep_input)) browser()
    
    reps_verbatim = gsub('rep','replicate ', rep_input)
    reps_verbatim = gsub('\\.', ' and ', reps_verbatim)
    reps = lapply(rep_input, function(x){x})
    names(reps) = reps_verbatim
    selectInput('a_select_scatterplot',
                'Replicates to compare in scatter plot', 
                choices = reps)
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
  
  # based on a_pulldown(), create slider for logFC
  output$a_logFC_slider <- renderUI({
    validate(need(input$a_file_pulldown_r != '', ""))
    
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
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    sliderInput("a_FDR_range", "FDR",
                min = 0, max = 1, value = c(0, 0.1), step = 0.01)
  })
  
  # based on a_pulldown(), create slider for logFC
  output$a_pvalue_slider <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    sliderInput("a_pvalue_range", "pvalue",
                min = 0, max = 1, value = c(0, 1), step = 0.01)
  })
  
  output$a_pf_loc_selection <- renderUI({
    #req(input$a_file_pulldown_r())
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
  
  # loading the data and getting the pulldown
  a_in_pulldown <- reactive({
    req(input$a_file_pulldown_r)
    d <- read_input(input$a_file_pulldown_r$datapath, sep = '\t', header = T)
    return(d)
  })
  
  # map accession_numbers to gene ids if needed
  a_orig_pulldown <- reactive({
    pulldown <- a_in_pulldown()
    if (pulldown$format$check$accession_rep == TRUE){
      pulldown$data <- map_gene_id(pulldown$data)
      }
    return(pulldown$data)
  })  
  
  
  # final pulldown formatted data.frame
  a_pulldown <- reactive({
    req(a_orig_pulldown(), a_in_pulldown())
    pulldown <- a_orig_pulldown()
    format <- a_in_pulldown()$format
    
    # moderated t.test still needed
    if (format$check$gene_rep | format$check$accession_rep){
      
      # set allowed column names
      allowed = unlist(format$allowed[unlist(format$check)])
      allowed_cols = lapply(allowed, function(x) grepl(x, colnames(pulldown)))
      allowed_vec = apply(do.call(rbind, allowed_cols), 2, any)
      allowed_vec = allowed_vec | 'gene' %in% colnames(pulldown)
      
      # ensure moderated t.test is only calculated on allowed columns
      pulldown = pulldown[,colnames(pulldown)[allowed_vec], with = FALSE]
      result = calc_mod_ttest(pulldown) 
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
    
    # pre-rendered messages
    msg1 = paste0(bold('Error:'),' None of the inpputed column names are allowed')
    msg2 = paste0(bold('Warning:'),' only ', length(accepted),'/',length(allowed_vec),' input column names were accepted.')
    msg3 = paste0('The following column names were invalid and discarded: ', italics(paste0(discarded, collapse = ', ')),'.')
    msg4 = paste0('See supplementary protocol for a description of allowed data inputs.')

    # no valid cols
    if (length(accepted) == 0){
      return(HTML(paste(msg1, msg4)))
    # enough valid but some invalid
    } else if (length(accepted) != length(allowed_vec)){
      return(HTML(paste(msg2, msg3, msg4)))
    } else return(NULL)
    
  })
 
  a_monitor_pulldown_mapping <- reactive({
    req(a_orig_pulldown())
    
    # mapping failed
    pulldown_mapping = a_orig_pulldown()
    failed = pulldown_mapping$accession_number[is.na(pulldown_mapping$gene)]
    absolute = paste0(length(failed),'/',nrow(pulldown_mapping))
    fraction = paste0(format(100*length(failed)/nrow(pulldown_mapping), digits = 3),'%')
    
    # messages
    msg1 = paste0(bold('Warning:'), absolute, ' (',fraction,') accesion_number(s) were not mapped to a gene(s).')
    msg2 = paste0('The following accesion_number(s) were not mapped:', italics(paste0(failed,collapse=', ')),'.')
    msg3 = paste0('These will need to have a manually specified "gene" column in for downstream analysis.')
    
    if (length(failed) > 0){
      return(HTML(paste(msg1, msg2, msg3)))
    } else return(NULL)
    
  }) 
 
  # function for handling uploaded genes
  a_genes_upload <- reactive({
    filepath = input$a_file_genes_rep$datapath
    if (!is.null(filepath)){
      genes = get_gene_lists(filepath)
      genes$data$dataset = paste('Genes', ifelse(is.null(genes$data$listName), 'Upload', as.character(genes$data$listName)))
      genes$data$col_significant = input$a_color_genes_upload_sig
      genes$data$col_other = input$a_color_genes_upload_insig
      genes$data$symbol = input$a_symbol_genes_upload
      genes$data$label = input$a_label_genes_upload
      return(genes)
    }
  })
  
  # needed for searching gene
  a_search_gene <- reactive({
    gene_in <- input$a_goi_search_rep
    req(gene_in)
    toupper(gene_in)
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
    mapping$alt_label = mapping$SNP
    mapping$col_significant = input$a_color_snp_sig
    mapping$col_other = input$a_color_snp_insig
    mapping$symbol = input$a_symbol_snp
    mapping$label = input$a_label_snp
    mapping$dataset =  paste('SNPs', mapping$listName)
    return(mapping)
  })
  
  # map inweb prorteins
  a_inweb_mapping <- reactive({
    req(input$a_bait_rep, a_pulldown())
    mapping = get_inweb_list(input$a_bait_rep)
    if (!is.null(mapping)){
        mapping = mapping[mapping$significant, ]
        mapping$col_significant = input$a_color_inweb_sig
        mapping$col_other = input$a_color_inweb_insig
        mapping$symbol = input$a_symbol_inweb
        mapping$label = input$a_label_inweb
        mapping$dataset = 'InWeb'
        return(mapping)
    } 
  })
  
  # map gwas catalouge
  a_gwas_catalogue_mapping <- reactive({
    req(input$a_gwas_catalogue, a_pulldown())
    genes = a_pulldown()$gene
    mapping = get_gwas_lists(input$a_gwas_catalogue, genes)
    if (!is.null(mapping)){
      mapping$col_significant = input$a_color_gwas_cat_sig
      mapping$col_other = input$a_color_gwas_cat_insig
      mapping$symbol = input$a_symbol_gwas_cat
      mapping$label = input$a_label_gwas_cat
      mapping$dataset = mapping$DISEASE.TRAIT
      mapping$alt_label = mapping$SNP
      return(mapping)
    }
  })
  
  
  # setup main gnomad mapping
  a_gnomad_mapping <- reactive({
    req(a_pulldown())
    pulldown = a_pulldown_significant()
    gnomad = merge(pulldown, gnomad_table, by = 'gene')
    gnomad$dataset = 'gnomAD'
    gnomad$alt_label = paste0('pLI=',gnomad$pLI)
    gnomad
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
  
  # hide show gnomad tab
  observe({
    if (!is.null(input$a_select_gnomad_pli_type)){
      if (input$a_select_gnomad_pli_type == 'none'){
        shinyjs::hide("a_slide_gnomad_pli_threshold_ui")
        #shinyjs::show("a_gnomad_colorscale_ui")
        #shinyjs::show("a_gnomad_colorscale_text_ui")
      } else {
        shinyjs::show("a_slide_gnomad_pli_threshold_ui")
        #shinyjs::hide("a_gnomad_colorscale_ui")
        #shinyjs::hide("a_gnomad_colorscale_text_ui")
      }
    }
  })
  
  
  
  #---------------------------------------------------------------
  # download different mappings and hide/show download buttons
  
  # download inweb mapping
  output$a_inweb_mapping_download <- downloadHandler(
    filename = function() {
      paste("inweb-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = a_pulldown_significant()
      inweb = a_inweb_mapping()[,c("dataset","gene")]
      mymerge = merge(pulldown, inweb, by = 'gene')
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  # download moderated t-test
  output$a_mttest_mapping_download <- downloadHandler(
    filename = function() {
      paste("pulldown-mttest",".csv", sep="")
    },
    content = function(file) {
      write.csv(a_pulldown_significant(), file, row.names = F)
    }
  )
  
  # download gene upload mapping
  output$a_gene_upload_mapping_download <- downloadHandler(
    filename = function() {
      paste("gene-upload-mapping",".csv", sep="")
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
      paste("snps-mapping",".csv", sep="")
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
      paste("gwas-catalogue-mapping",".csv", sep="")
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
      paste("gnomad-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = a_pulldown_significant()
      gnomad = a_gnomad_mapping_threshold()[,c('gene','pLI','oe_lof','oe_lof_lower','oe_lof_upper','gene_id')]
      mymerge = merge(pulldown, gnomad, by = 'gene')
      write.csv(mymerge, file, row.names = F)
    }
   )
  
  # download protein family mapping? gene annotations
  output$a_pathway_mapping_download <- downloadHandler(
    filename = function() {
      paste("gene-set-annotations-mapping",".csv", sep="")
    },
    content = function(file) {
      pulldown = a_pulldown_significant()
      pathway = a_pathway_mapping()[,c('gene','pathway','Freq')]
      mymerge = merge(pulldown, pathway, by = 'gene')
      write.csv(mymerge, file, row.names = F)
    }
  )
  
  # # # venn diagrams # # #
  
  # venn diagram inweb mapping for inweb
  output$a_inweb_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("inweb_venn_mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_inweb_calc_hyper()$venn
      write.csv(venn_to_table(venn), file, row.names = F)
   }
  )
  
  # venn diagram mapping for gnomad
  output$a_gnomad_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("gnomad_venn_mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_gnomad_calc_hyper()$venn
      write.csv(venn_to_table(venn), file, row.names = F)
    }
  )
  
  # venn diagram mapping for gwas catalogue
  output$a_gwas_catalogue_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("a_gwas_catalogue_venn_mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_gwas_catalogue_mapping_venn()
      write.csv(venn_to_table(venn), file, row.names = F)
    }
  )
  
  # venn diagram mapping snps
  output$a_snp_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("a_snp_venn_mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_snp_venn()
      write.csv(venn_to_table(venn), file, row.names = F)
    }
  )
  
  # venn diagram mapping for uploaded genes
  output$a_genes_upload_venn_mapping_download <- downloadHandler(
    filename = function() {
      paste("a_genes_upload_venn_mapping",".csv", sep="")
    },
    content = function(file) {
      venn = a_genes_upload_venn()
      write.csv(venn_to_table(venn), file, row.names = F)
    }
  )
  
  # show/hide data download buttons
  observeEvent(input$a_file_pulldown_r, {shinyjs::toggle(id="a_mttest_mapping_download", condition=!is.null(input$a_file_pulldown_r))})
  observeEvent(input$a_bait_rep, {shinyjs::toggle(id="a_inweb_mapping_download", condition=!is.null(a_pulldown_significant()) & any(input$a_bait_rep %in% hash::keys(inweb_hash)))})
  observe({shinyjs::toggle(id="a_snp_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_file_SNP_rep$datapath))})
  observe({shinyjs::toggle(id="a_gene_upload_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_file_genes_rep))})
  observe({shinyjs::toggle(id="a_gwas_catalogue_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_gwas_catalogue))})
  observe({shinyjs::toggle(id="a_gnomad_mapping_download", condition=!is.null(a_pulldown_significant()) & input$a_select_gnomad_pli_type == 'threshold')})
  observe({shinyjs::toggle(id="a_pathway_mapping_download", condition=!is.null(a_pulldown_significant()))})
  
  # venn diagrams
  observeEvent(input$a_bait_rep, {shinyjs::toggle(id="a_inweb_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & any(input$a_bait_rep %in% hash::keys(inweb_hash)))})
  observe({shinyjs::toggle(id="a_snp_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_file_SNP_rep$datapath))})
  observe({shinyjs::toggle(id="a_genes_upload_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_file_genes_rep))})
  observe({shinyjs::toggle(id="a_gwas_catalogue_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(input$a_gwas_catalogue))})
  observe({shinyjs::toggle(id="a_gnomad_venn_mapping_download", condition=!is.null(a_pulldown_significant()) & !is.null(a_gnomad_mapping_threshold()) & input$a_select_gnomad_pli_type == 'threshold')})
  
  # warning boxes
  # observeEvent(!is.null(a_pulldown_significant()),{shinyjs::toggle(id="box_mapping_warning_ui")})
  
  # show/hide plot download buttons
  observeEvent(!is.null(a_pulldown_significant()),{
    shinyjs::show("a_volcano_plot_download")
    shinyjs::show("a_scatter_plot_download")
    shinyjs::show("a_integrated_plot_download")
    shinyjs::show("a_pathway_plot_download")
  })
  
  #--------------------------------------------------------
  # venn digrams and hypergeometric testing
  
  # INWEB
  # inweb hypergeometric overlap
  a_inweb_calc_hyper <- reactive({
    req(input$a_bait_rep, a_pulldown_significant())
    inweb_output = get_inweb_list(input$a_bait_rep)
    if (!is.null(inweb_output)){
      
      #browser()
      
      # gather all inweb data
      inweb_list = data.frame(listName="InWeb", inweb_output)
      inweb_intersect = data.frame(listName="InWeb", intersectN=T)
      data = a_pulldown_significant()
      
      #sta
      #data$gene[!sigDf$gene == data$gene]
      #sigDf$gene[!sigDf$gene == data$gene]
      #data$gene[grepl('or',data$gene)]
      
      # compile venn diagram information
      hyper = calc_hyper(data, inweb_list, inweb_intersect, bait = input$a_bait_search_rep)
      hyper[['venn']][['A']] <- hyper$genes$InWeb$success_genes # pulldown
      hyper[['venn' ]][['B']] <- hyper$genes$InWeb$sample_genes # inweb
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
  a_inweb_venn_verbatim <- reactive({
    req(a_pulldown_significant(), a_inweb_calc_hyper(), input$a_bait_rep)
    thresholds = paste(monitor_significance_thresholds()$sig, monitor_logfc_threshold()$sig, sep =', ')
    hyper = a_inweb_calc_hyper()
    A <- paste0("A = pull down subsetted by ", thresholds, " &#40;", bold(hyper$statistics$success_count), "&#41;")
    B <- paste0("B = ", bold(input$a_bait_rep)," InWeb interactors", " &#40;", bold(hyper$statistics$sample_count), "&#41;")
    total <- paste0("Total population = pull down &cap; InWeb &#40;", bold(hyper$statistics$population_count), "&#41;")
    return(list(A=A, B=B, total=total))
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
  output$a_inweb_venn_verbatim_ui <- renderUI({
    output <- a_inweb_venn_verbatim()
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
    hyper = calc_hyper(pulldown, genes_selected, intersect_selected, input$a_bait_search_rep)
    
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
    A <- paste0("A = Pull down subsetted by ", thresholds, " &#40;", bold(length(diagram[[1]])), "&#41;")
    B <- paste0("B = Genes in ",italics(selected)," &#40;", bold(length(unique(diagram[[2]]))), "&#41;")
    total <- paste0("Total population = pull down &#40;", bold(nrow(pulldown)), "&#41;")
    return(list(A=A, B=B, total=total))
  })
  
  # Send to UI
  output$a_genes_upload_venn_verbatim_ui <- renderUI({
    output <- a_genes_upload_venn_verbatim()
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
    A <- paste0("A = pull down subsetted by ", thresholds, " &#40;", bold(length(diagram[[1]])), "&#41;")
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
    A <- paste0("A = pulldown subsetted by ", thresholds, " &#40;", bold(length(diagram[[1]])), "&#41;")
    B <- paste0("B = Genes mapped from GWAS catalog &#40;", bold(length(unique(diagram[[2]]))), "&#41;")
    total <- paste0("Total population = pulldown", " &#40;", bold(nrow(pulldown)), "&#41;")
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
    A <- paste0("A = pull down subsetted by ", tresholds, " &#40;", bold(hyper$statistics$success_count), "&#41;")
    B <- paste0("B = gnomAD genes with pLI ≥", bold(input$a_slide_gnomad_pli_threshold)," &#40;", bold(hyper$statistics$sample_count), "&#41;")
    total <- paste0("Total population = pull down &cap; gnomAD &#40;", bold(hyper$statistics$population_count), "&#41;")
    return(list(A=A, B=B, total=total))
  })
  
  # print to ui
  output$a_gnomad_venn_verbatim_ui <- renderUI({
    output <- a_gnomad_venn_verbatim()
    HTML(paste(output$total, output$A, output$B, sep = "<br/>"))
  })
  
  
  #---------------------------------------------------------------
  # gnomad plot clicking integration
  
  a_table_gnomad_constraints <- reactive({
    hover_index = event_data("plotly_click", source = "Multi_VolcanoPlot")
    if (!is.null(hover_index)){
      if (hover_index$key %in% gnomad_table$gene){
        tabl = get_gnomad_constraints(hover_index$key)
        return(tabl)
      }
    }
  })
  
  # render text for gnomad status
  output$a_gnomad_constraints_available_ui <- renderUI({
    gene = event_data("plotly_click", source = "Multi_VolcanoPlot")$key
    if (!is.null(gene)){
      if (gene %in% gnomad_table$gene){
        return(HTML(paste(bold(gene),'constraint info from gnomAD 2.1.1.')))
      } else {
        return(HTML(paste('No constraint info for', bold(gene), 'in gnomAD 2.1.1.')))
      }
    }
    return('Nothing selected. Click a plot point.')
  })
  
  # render table
  output$a_table_gnomad_constraints_ui <- renderTable(a_table_gnomad_constraints())
  
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
    p <- plot_overlay(p, as.bait(input$a_bait_search_rep)) # add bait
    return(p)
  })
  
  # basic volcano plot
  a_vp_layerx <- reactive({
    p <- a_vp_gg()
    p <- make_interactive(p, legend = T)
    if (input$a_goi_search_rep != '') p <- add_markers_search(p, a_search_gene())
    p <- add_hover_lines_volcano(p, line_pvalue = input$a_pval_thresh, line_logfc = input$a_logFC_thresh, logfc_direction = input$a_logfc_direction, sig_type = input$a_significance_type)
    p <- add_layout_html_axes_volcano(p, 500*0.8, 625*0.8)
    return(p)
  })
  
  
  # basic plot gene count summary
  a_verbatim_count <- reactive({
    d <- a_pulldown_significant()
    HTML(paste(bold(sum(d$significant)), 'proteins out of', bold(nrow(d)), 'proteins enriched.'))
  })
  
  # basic plot significance text
  a_vp_count_text <- reactive({
    if(!is.null(a_verbatim_count())){
      enriched <- paste(c('Enrichment threshold:',monitor_significance_thresholds()$sig, 'and', monitor_logfc_threshold()$sig))
      HTML(enriched)
    }
  })
 
  
  a_sp_gg_all <- reactive({
    # handle all scatter plots
    req(a_pulldown_significant())
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
    p1 = plot_overlay(p1, as.bait(input$a_bait_search_rep))
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
    if (input$a_goi_search_rep != '') p1 <- add_markers_search(p1, a_search_gene())
    p1 = add_layout_html_axes_scatterplot(p1, paste0('r=',r))
    p1 = add_line_unity(p1)
    #p1 = add_line_lm(p1, x=rep[1], y=rep[2])
    
  })
  

  #---------------------------------------------------------------------
  # integrated plotting
  
  # generate plot in ggformat
  a_integrated_plot_gg <- reactive({
    p = a_vp_gg()
    if (!is.null(input$a_gwas_catalogue)) if (input$a_gwas_catalogue != '') p = plot_overlay(p, list(gwas=a_gwas_catalogue_mapping()))
    if (!is.null(input$a_bait_rep)) if (input$a_bait_rep %in% hash::keys(inweb_hash)) p = plot_overlay(p, list(inweb=a_inweb_mapping()))
    if (!is.null(input$a_file_SNP_rep)) if (input$a_overlay_snp) {p = plot_overlay(p, list(snps=a_snp_mapping()))}
    if (!is.null(input$a_file_genes_rep)) if (input$a_overlay_genes_upload) {p = plot_overlay(p, list(upload=a_genes_upload()$data))}
    if (!is.null(input$a_select_gnomad_pli_type)) if (input$a_select_gnomad_pli_type == 'threshold') p = plot_overlay(p, list(gnomad=a_gnomad_mapping_threshold()))
    p$overlay <- collapse_labels(p$overlay)
    p
    
  })
  
  # convert into plotly graphics
  a_integrated_plot <- reactive({
    sig_label = '(enriched)' #paste0(monitor_significance_thresholds()$sig) #, ', ', monitor_logfc_threshold_non_html()$sig)
    p <- a_integrated_plot_gg()
    p <- make_interactive(p, source = "Multi_VolcanoPlot", legend = T, sig_text = sig_label)
    p <- add_hover_lines_volcano(p, line_pvalue = input$a_pval_thresh, line_logfc = input$a_logFC_thresh, logfc_direction = input$a_logfc_direction,  sig_type = input$a_significance_type)
    if (input$a_goi_search_rep != '') p <- add_markers_search(p, a_search_gene())
    p <- add_layout_html_axes_volcano(p, 550, 650) # error in searching overlay here when layout width/height supplied. 
    p
  })
  
  # download integrated plot graphics.
  input_integrated_plot_gg <- function(){a_integrated_plot_gg()}
  output$a_integrated_plot_download = downloadHandler(
    filename = 'integrated-plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot =  input_integrated_plot_gg(), device = device)
    })
  
  # download pathway annoation plot
 input_pathway_plot_gg <- function(){a_pathway_plot_gg()}
  output$a_pathway_plot_download = downloadHandler(
    filename = 'gene-set-annotation-volcano-plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot =  input_pathway_plot_gg(), device = device)
    })
  
  
  # download basic scatter plot
  input_sp_gg <- function(){a_sp_gg()}
  output$a_scatter_plot_download = downloadHandler(
    filename = 'scatter-plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot =  input_sp_gg(), device = device)
    })
  
  # download basic scatter plot
  input_vp_gg <- function(){a_vp_gg()}
  output$a_volcano_plot_download = downloadHandler(
    filename = 'basic-volcano-plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot =  input_vp_gg(), device = device)
    })
  
  
  

  #---------------------------------------------------------------------
  # integrated plotting
  
  # assign frequency 
  a_pathway_mapping_assign_freq <- reactive({
    req(a_pulldown_significant(), input$a_pf_loc_option, )
    db = input$a_pf_loc_option
    pulldown <- a_pulldown_significant()
    pulldown <- pulldown[pulldown$significant, ]
    overlap <- get_pathways(db, pulldown$gene)
    overlap <- assign_freq(overlap, 'pathway')
    overlap = merge(overlap, pulldown)
    overlap
  })
  
  # load in data and preset colors in a seperate
  # reactive to reduce overhead time
  a_pathway_mapping_initial <- reactive({
    req(a_pulldown_significant(), a_pathway_mapping_assign_freq())
    
    
    #  # get raw data and assign frequency count
    overlap <- a_pathway_mapping_assign_freq()
    # assign color scheme
    colors = as.data.frame(overlap[,c('pathway','Freq')])
    colors = colors[!duplicated(colors), ]
    colors$color = NA
    #colors$color = ifelse(colors$Freq <= 1, 'grey', NA)
    colors$color[is.na(colors$color)] <- color_distinct()[1:sum(is.na(colors$color))]
    # merge with overlap
    overlap = merge(overlap, colors[,c('pathway','color')], by = 'pathway')
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
    counts = data.frame(pathway=overlay$pathway, Freq=overlay$Freq)
    counts = counts[!duplicated(counts),]
    revcumsum = cumsum(rev(table(counts$Freq)))
    return(revcumsum)
  })
  
  # calculate the lowest allowed frequency of pathway
  # that may appear in the plot
  a_pathway_mapping_freq_lowest_allowed <- reactive({
    req(a_pathway_mapping_freq_revcumsum())
    revcumsum = a_pathway_mapping_freq_revcumsum()
    lowest_allowed_freq = as.numeric(names(rev(revcumsum[revcumsum < 100]))[1])
    #if (is.na(lowest_allowed_freq)) lowest_allowed_freq <- min(as.numeric(names(revcumsum)))
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
      overlay$col_significant = overlay$color
      overlay$col_other = overlay$color
      overlay$symbol = 'square'
      overlay$shape = 'square'
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
  


  # make the ggplot
  a_pathway_plot_gg <- reactive({
    req(a_pulldown_significant())
      p <- a_vp_gg()
      if (nrow(a_pathway_mapping_subset()) > 0) {p <- plot_overlay(p, list(pathway=a_pathway_mapping_subset()))}
      p
  })
  
  # convert to plotly
  a_pathway_plot <- reactive({
    req(a_pulldown_significant(), a_pathway_plot_gg(), a_pathway_mapping_initial())
  
    p <- a_pathway_plot_gg()
    
    # for now, we order overlay AFTER plot_overlay() has been called since 
    # the legend_order column would otherwise be disrupted through a merge operation.
    # see github issues for more details. In the future, this should be a reactive.
    p$overlay$legend_order = unlist(ifelse(input$a_pathway_mapping_type_sort == 'freq',
                                    list(rev(order(p$overlay$size))), list(order(p$overlay$dataset))))
    
    p <- make_interactive(p, legend = T)
    if (input$a_goi_search_rep != '') p <- add_markers_search(p, a_search_gene())
    if (!is.null(input$a_pathway_mapping_search)) p <- add_markers_search_pathway(p, input$a_pathway_mapping_search, mapping = a_pathway_mapping_initial())
    p <- add_hover_lines_volcano(p, line_pvalue = input$a_pval_thresh, line_logfc = input$a_logFC_thresh, logfc_direction = input$a_logfc_direction, sig_type = input$a_significance_type)
    p <- add_layout_html_axes_volcano(p, 500, 875)
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
    validate(need(input$a_file_pulldown_r != '', ""))
    a_vp_colorbar()
  })
  
  output$FDR_colorbar_integrated <- renderPlot({
    validate(need(input$a_file_pulldown_r != '', ""))
    a_vp_colorbar()
  })
  

  # the actual volcano plot outputted to the user
  output$VolcanoPlot <- renderPlotly({
    validate(need(input$a_file_pulldown_r != '', "Upload file"))
      a_vp_layerx()
  })
  
  output$VolcanoPlotPathway <- renderPlotly({
    req(a_pathway_plot())
    a_pathway_plot()
  })
  
  
  output$a_verbatim_count_ui <- renderUI({
    validate(need(input$a_file_pulldown_r != '', " "))
    a_verbatim_count()
  })
  
  output$VP_count_text <- renderUI({
    validate(need(input$a_file_pulldown_r != '', " "))
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
  
  
  output$ScatterPlot <- renderPlotly({
    validate(need(input$a_file_pulldown_r != '', "Upload file"))
    a_sp()
  })
  
  output$multi_FDR_colorbar <- renderPlot({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    a_multi_vp_colorbar()
  })
  
  output$Multi_VolcanoPlot <- renderPlotly({
    #validate(need(input$a_file_pulldown_r != '', "Upload file"))
    req(a_pulldown_significant)
    a_integrated_plot()
  })
  
  
  
  
  ##### GENERAL #####
  #documentation
  
  getPage_guide<-function() {
    
    
    
    
    #return(tags$iframe(src = "welcome_guide_200415.pdf",
    #                   style="width:100%;",  #frameborder="0"
    #                   height = "3100px"))
  }
  
  
  
  output$how_to_guide <- renderUI({
    getPage_guide()
  })
  
  
  # output$documentation <- renderUI({
  #   return(includeHTML("documentation/doc_0316.html")
  #   )
  # })
  
})



