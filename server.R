#Created 9/10/16 by April Kim
#Server file for Shiny app Genoppi
options(stringsAsFactors = F)
library(shiny)
library(shinyjs)
library(plotly)
library(limma)
library(stringr)
library(VennDiagram)
flog.threshold(ERROR)
library(rmarkdown)
library(plyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
source("functions.R")

shinyServer(function(input, output, session){
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
                   "Grayscale" = "cbf"),
                 inline = T)
  })
  
  output$a_bait_layer <- renderUI({
    textInput("a_bait_rep", "Input HGNC symbol to search for InWeb protein interactors (e.g. ZBTB7A)")
  })
  
  output$a_bait_venndiagram <- renderUI({
    textInput("a_bait_vennd", "Input HGNC symbol to search for InWeb protein interactors (e.g. ZBTB7A)")
  })
  
  output$a_GOI_search <- renderUI({
    textInput("a_goi_search_rep", "Search for gene (e.g. SHH)")
  })
  
  output$FDR_thresh <- renderUI({
    sliderInput("a_fdr_thresh", "FDR threshold",
                min = 0, max = 1, value = 0.1, step = 0.01)
  })
  
  output$PVal_thresh <- renderUI({
    sliderInput("a_pval_thresh", "p-value threshold",
                min = 0, max = 1, value = 0.05, step = 0.001)
  })

  # based on a_pulldown(), create slider for logFC
  output$logFC_thresh <- renderUI({
    if(!is.null(input$a_file_pulldown_r)){
      df <- a_pulldown()
      min_logFC <- min(df$logFC)
      min_logFC <- signif(min_logFC-0.5, 1)
      max_logFC <- max(df$logFC)
      max_logFC <- signif(max_logFC+0.5, 1)
      combined_max <- max(-(min_logFC), max_logFC)
      numericInput("a_logFC_thresh", "logFC threshold",
                  min = 0, max = combined_max, value = 1, step = 0.1)
    }else
      numericInput("a_logFC_thresh", "logFC threshold",
                  min = 0, max = 1, value = 0.1, step = 0.1)
  })
  
  output$a_text_label <- renderUI({
    radioButtons('a_marker_text', 'Turn on/off labels',
                 c(On = 'yes_label',
                   Off = 'no_label'),
                 inline = T
    )
  })
  
  output$a_SNP_file <- renderUI({
    fileInput('a_file_SNP_rep', 'File containing list of SNPs, one ID per line (e.g. rs12493885)',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv')
    )
  })
  
  output$a_SNP_file_vennd <- renderUI({
    fileInput('a_file_SNP_vennd', 'File containing list of SNPs, one ID per line (e.g. rs11172113)',
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
    fileInput('a_file_genes_rep', 'File containing at least 2 genes with header, one HGNC symbol per line (e.g. TINMAN)',
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
  
  output$a_plot_button <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    if(!is.null(a_bait_gene_layer()) | !is.null(a_snp()) | !is.null(a_upload_genes())){
      actionButton("a_make_plot", "Generate plots")
    }
  })
  
  output$a_vennd_button3 <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', ""),
      need(input$a_bait_vennd != '', "")
    )
    if(!is.null(a_bait_gene_vennd())){
      actionButton("a_make_vennd_bait", "Bait")
    }
  })
  
  output$a_vennd_button1 <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    if(!is.null(a_snp_vennd())){
      actionButton("a_make_vennd_snp", "SNP")
    }
  })
  
  output$a_vennd_button2 <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    if(!is.null(a_upload_genes_vennd())){
      actionButton("a_make_vennd_goi", "GOI")
    }
  })
  
  # based on a_pulldown(), create slider for logFC
  output$a_logFC_slider <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    if(!is.null(a_pulldown())){
      input_file <- a_pulldown()
      df <- input_file
      min_logFC <- min(df$logFC)
      min_logFC <- signif(min_logFC-0.5, 1)
      max_logFC <- max(df$logFC)
      max_logFC <- signif(max_logFC+0.5, 1)
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
  
  #create slider for FDR
  output$a_BPF_FDR_slider <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
      sliderInput("a_BPF_FDR_range", "FDR",
                  min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
    
  })
  
  #create slider for pvalue
  output$a_BPF_pvalue_slider <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
      sliderInput("a_BPF_pvalue_range", "pvalue",
                  min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
    
  })
  
  # based on a_pulldown(), create slider for BPF logFC
  output$a_BPF_logFC_slider <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    if(!is.null(a_pulldown())){
      input_file <- a_pulldown()
      df <- input_file
      min_logFC <- min(df$logFC)
      min_logFC <- signif(min_logFC-0.5, 1)
      max_logFC <- max(df$logFC)
      max_logFC <- signif(max_logFC+0.5, 1)
      sliderInput("a_BPF_logFC_range", "logFC",
                  min = min_logFC, max = max_logFC, value = c(1, max_logFC), step = 0.1)
    }
  })
  
  output$a_BPF_marker_size <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    radioButtons('a_BPF_option', 'Turn on/off marker sizing',
                 c(On = 'change_m_BPF',
                   Off = 'static_m_BPF'),
                 inline = T
    )
  })
  
  output$a_BPF_freq <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    if(!is.null(input$a_BPF_option)){
      inBPFmarker <- input$a_BPF_option
      if (inBPFmarker == "change_m_BPF") {
        sliderInput("a_BPF_marker_freq", "Marker size",
                    min = 1, max = 40, value = 1, step = 1)     
      }
    }
  })
  
  output$a_BPF_text <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    HTML("<b>User-defined significance thresholds:</b>")
  })
  
  output$a_BPF_button <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    if(!is.null(a_pulldown())){
      actionButton("a_make_bpf", "Generate PF plot")
    }
  })
  
  #slider for rep1
  output$a_BPF_rep1_slider <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    d <- a_pulldown()
    d_col <- colnames(d)
    if("rep1" %in% d_col & "rep2" %in% d_col){
      min_rep1 <- min(d$rep1)
      print(paste0("orig min rep1 ", min_rep1))
      min_rep1 <- signif(min_rep1-0.5, 1)
      print(paste0("rounded min rep1 ", min_rep1))
      max_rep1 <- max(d$rep1)
      max_rep1 <- signif(max_rep1+0.5, 1)
      sliderInput("a_BPF_rep1_range", "rep1",
                  min = min_rep1, max = max_rep1, value = c(-1, 1), step = 0.01, sep = '', pre = NULL, post = NULL)
    }
  })
  
  #slider for rep2
  output$a_BPF_rep2_slider <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    d <- a_pulldown()
    d_col <- colnames(d)
    if("rep1" %in% d_col & "rep2" %in% d_col){
      min_rep2 <- min(d$rep2)
      min_rep2 <- signif(min_rep2-0.5, 1)
      max_rep2 <- max(d$rep2)
      max_rep2 <- signif(max_rep2+0.5, 1)
      sliderInput("a_BPF_rep2_range", "rep2",
                  min = min_rep2, max = max_rep2, value = c(-1, 1), step = 0.01, sep = '', pre = NULL, post = NULL)
    }
  })
  
  output$a_BPF_marker_size_sp <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    d <- a_orig_pulldown()
    d_col <- colnames(d)
    if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
       "rep1" %in% d_col & "rep2" %in% d_col){
      radioButtons('a_BPF_option_sp', 'Turn on/off marker sizing',
                   c(On = 'change_m_BPF',
                     Off = 'static_m_BPF'),
                   inline = T
      )
    } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
      validate(
        return(NULL)
      )
    } else if("rep1" %in% d_col & "rep2" %in% d_col){
      radioButtons('a_BPF_option_sp', 'Turn on/off marker sizing',
                   c(On = 'change_m_BPF',
                     Off = 'static_m_BPF'),
                   inline = T
      )
    }
  })
  
  output$a_BPF_freq_sp <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    if(!is.null(input$a_BPF_option_sp)){
      inBPFmarker <- input$a_BPF_option_sp
      if (inBPFmarker == "change_m_BPF") {
        sliderInput("a_BPF_marker_freq_sp", "Marker size",
                    min = 1, max = 40, value = 1, step = 1)     
      }
    }
  })
  
  output$a_BPF_text_sp <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    d <- a_orig_pulldown()
    d_col <- colnames(d)
    if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
       "rep1" %in% d_col & "rep2" %in% d_col){
      HTML("<b>User-defined significance thresholds:</b>")
    } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
      validate(
        need("rep1" %in% d_col & "rep2" %in% d_col, "Must have rep1 and rep2 values.")
      )
    } else if("rep1" %in% d_col & "rep2" %in% d_col){
      HTML("<b>User-defined significance thresholds:</b>")
    }
  })
  
  output$a_BPF_button_sp <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    d <- a_orig_pulldown()
    d_col <- colnames(d)
    if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
       "rep1" %in% d_col & "rep2" %in% d_col){
      actionButton("a_make_bpf_sp", "Generate PF plot")
    } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
      return(NULL)
    } else if("rep1" %in% d_col & "rep2" %in% d_col){
      actionButton("a_make_bpf_sp", "Generate PF plot")
    }
  })
  
  
  output$a_prot_fam_db <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    selectInput('a_pfam_db', 'Protein families', colnames(prot_fam), multiple=TRUE, selectize=TRUE)
  })
  
  output$a_text_prot_fam_db <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    radioButtons('a_marker_text_prot_fam_db', 'Turn on/off labels',
                 c(On = 'yes_label',
                   Off = 'no_label'),
                 inline = T
    )
  })
  
  observe({
    if(input$basic == "p3" | input$basic == "p4" | input$basic == "p5"){
      shinyjs::hide("colorscheme")
      shinyjs::hide("a_fdr_thresh")
      shinyjs::hide("a_pval_thresh")
      shinyjs::hide("a_logFC_thresh")
    }else {
      shinyjs::show("colorscheme")
      shinyjs::show("a_fdr_thresh")
      shinyjs::show("a_pval_thresh")
      shinyjs::show("a_logFC_thresh")
    }
  })
  
  a_in_pulldown <- reactive({
    if(!is.null(input$a_file_pulldown_r)){
      pulldown <- input$a_file_pulldown_r
      d <- fread(pulldown$datapath, header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
      d <- na.omit(d)
    }
  })
  
  a_orig_pulldown <- reactive({
    if(!is.null(a_in_pulldown())){
      d <- a_in_pulldown()
      d_col <- colnames(d)
      if("gene" %in% d_col & "accession_number" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      } else if("accession_number" %in% d_col){
        withProgress(message = 'Mapping UniProt IDs to HGNC symbols',
                     detail = 'Hold please', value = 0, {
                       write.table(d$accession_number, "scripts/gene-tools-master/map/in.txt", append = F, quote = F,
                                   row.names = F, col.names = F)
                       incProgress(0.6)
                       system("python scripts/gene-tools-master/map/map.py")
                       incProgress(0.8)
                       d1 <- fread("scripts/gene-tools-master/map/results.txt", header = TRUE,
                                   sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
                       colnames(d1) <- c("uniprot_id", "gene", "method")
                       df <- merge(d, d1, by.x = "accession_number", by.y = "uniprot_id")
                       df <- subset(df, select=-c(method))
                       incProgress(0.9)
                       df
                     })
      } else if("gene" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      }
      df
    }
  })
  
  a_pulldown <- reactive({
    if(!is.null(a_orig_pulldown())){
      d <- a_orig_pulldown()
      d_col <- colnames(d)
      if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
         "rep1" %in% d_col & "rep2" %in% d_col){
        df1 <- d
      }else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
        df1 <- d
      }else if("rep1" %in% d_col & "rep2" %in% d_col){
        df <- d[,c("gene","rep1","rep2")]
        df1 <- calculate_moderated_ttest(df)
      }
      df1
    }
  })
  
  a_converted <- reactive({
    if(!is.null(a_orig_pulldown())){
      d <- fread("scripts/gene-tools-master/map/results.txt", header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
    }
  })
  
  a_snp <- reactive({
    snp <- input$a_file_SNP_rep
    if(is.null(snp)){
      return(NULL)
    } else{
      df <- read.csv(snp$datapath, header = FALSE)
      df
    }
  })
  
  a_snp_vennd <- reactive({
    snp <- input$a_file_SNP_vennd
    if(is.null(snp)){
      return(NULL)
    } else{
      df <- read.csv(snp$datapath, header = FALSE)
      df
    }
  })
  
  a_upload_genes <- reactive({
    genes <- input$a_file_genes_rep
    if (is.null(genes)){
      return(NULL)
    } else{
      df <- fread(genes$datapath, header = T, fill = T,
                  sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
    }
  })
  
  a_genes_uploaded <- reactive({
    if(!is.null(a_upload_genes())){
      genes <- a_upload_genes()
      genes <- as.data.frame(sapply(genes, toupper)) 
      genes
    }
  })
  
  a_upload_genes_vennd <- reactive({
    genes <- input$a_file_genes_vennd
    if (is.null(genes)){
      return(NULL)
    } else{
      df <- fread(genes$datapath, header = T, fill = T,
                  sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
    }
  })
  
  a_genes_uploaded_vennd <- eventReactive(input$a_make_vennd_goi, { 
    if(!is.null(a_upload_genes_vennd())){
      genes <- a_upload_genes_vennd()
      genes <- as.data.frame(sapply(genes, toupper)) 
      genes
    }
  })
  
  output$a_goi_num_inputs <- renderUI({
    validate(
      need(!is.null(a_genes_uploaded_vennd()), "")
    )
    d <- a_pulldown()
    gene_interest <- a_genes_uploaded_vennd()
    d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )  
    choices <- names(d_g2s)
    choices <- append(choices, "total")
    list_count <- length(d_g2s)
    if(list_count>0){
      selectInput("a_goi_num_inputs", "GOI list",
                  choices = choices, multiple = F)
    }
  })
  
  #population_bait -- pull down overlap inweb
  population_bait <- reactive({
    present_in_inweb <- a_pulldown()
    population_bait <- subset(present_in_inweb, present_in_inweb$gene %in% inweb_combined$V1)
  })
  
  population_GOI <- reactive({
    present_in_hum_genome <- a_pulldown()
    hgnc <- toupper(human_genome$HGNCsymbol)
    population_hum_genome <- subset(present_in_hum_genome, present_in_hum_genome$gene %in% hgnc)
  })
  
  population_SNP <- reactive({
    present_in_hum_genome <- a_pulldown()
    hgnc <- toupper(human_genome$HGNCsymbol)
    population_hum_genome <- subset(present_in_hum_genome, present_in_hum_genome$gene %in% hgnc)
  })
  
  #success in population_bait -- within user FDR and logFC cutoff in population_bait
  success_population_bait <- reactive({ #eventReactive(input$a_logFC_range | input$a_FDR_range, {
    pullDown_in_inweb <- population_bait()
    success_population <- subset(pullDown_in_inweb, logFC < input$a_logFC_range[2] & logFC > input$a_logFC_range[1] &
                                   FDR < input$a_FDR_range[2] & FDR > input$a_FDR_range[1] &
                                     pvalue < input$a_pvalue_range[2] & pvalue > input$a_pvalue_range[1])
    rownames(success_population) <- NULL 
    success_population
  })
  
  success_population_GOI <- reactive({ #eventReactive(input$a_logFC_range | input$a_FDR_range, {
    pullDown_in_hum_genome <- population_GOI()
    success_population <- subset(pullDown_in_hum_genome, logFC < input$a_logFC_range[2] & logFC > input$a_logFC_range[1] &
                                        FDR < input$a_FDR_range[2] & FDR > input$a_FDR_range[1] &
                                       pvalue < input$a_pvalue_range[2] & pvalue > input$a_pvalue_range[1])
    rownames(success_population) <- NULL 
    success_population
  })
  
  success_population_SNP <- reactive({ #eventReactive(input$a_logFC_range | input$a_FDR_range | input$a_pvalue_range, {
    pullDown_in_hum_genome <- population_SNP()
    success_population <- subset(pullDown_in_hum_genome, logFC < input$a_logFC_range[2] & logFC > input$a_logFC_range[1] &
                                       FDR < input$a_FDR_range[2] & FDR > input$a_FDR_range[1] &
                                       pvalue < input$a_pvalue_range[2] & pvalue > input$a_pvalue_range[1])
    rownames(success_population) <- NULL 
    success_population
  })
  
  a_bait_gene_layer <- reactive({
    bait_in <- input$a_bait_rep
    if (is.null(bait_in) | bait_in == "" ){
      return(NULL)
    } else{
      bait <- bait_in
      bait <- toupper(bait)
    }
  })
  
  a_bait_gene_vennd <- reactive({
    bait_in <- input$a_bait_vennd
    if (is.null(bait_in) | bait_in == "" ){
      return(NULL)
    } else{
      bait <- bait_in
      bait <- toupper(bait)
    }
  })
  
  a_search_gene <- reactive({
    gene_in <- input$a_goi_search_rep
    if (is.null(gene_in) | gene_in == "" ){
      return(NULL)
    } else{
      gene <- gene_in
      gene <- toupper(gene)
    }
  })
  
  a_bait_friends <- reactive({
    if(!is.null(a_bait_gene_layer())){
      withProgress(message = 'Finding bait interactors in InWeb', 
                   detail = 'Hold please', value = 0, {
                     bait <- a_bait_gene_layer()
                     bait_inweb3 <-
                       system(paste("grep -w", bait, "data/InWeb3_interactions.txt | sed -e 's/(//g' | tr '\t' '\\n' | sort -u | grep -v", bait, sep = " "), intern = TRUE)
                     incProgress(0.5)
                     bait_inweb_im <- 
                       system(paste("zgrep -w", bait, "data/core.psimitab.gz | awk -v FS=\"(uniprotkb:|gene name)\" '{print $6, $9}' | sed -e 's/(//g' | tr ' ' '\\n' | sort -u | grep -v", bait, sep = " "), intern = TRUE)
                     incProgress(0.8)
                   })
      bait_interactors <- c(bait_inweb_im, bait_inweb3)
      bait_interactors <- unique(sort(bait_interactors))
      bait_interactors
    }
  })
  
  a_bait_friends_vennd <- eventReactive(input$a_make_vennd_bait, { 
    if(!is.null(a_bait_gene_vennd())){
      withProgress(message = 'Finding bait interactors in InWeb', 
                   detail = 'Hold please', value = 0, {
                     bait <- a_bait_gene_vennd()
                     bait_inweb3 <-
                       system(paste("grep -w", bait, "data/InWeb3_interactions.txt | sed -e 's/(//g' | tr '\t' '\\n' | sort -u | grep -v", bait, sep = " "), intern = TRUE)
                     incProgress(0.5)
                     bait_inweb_im <- 
                       system(paste("zgrep -w", bait, "data/core.psimitab.gz | awk -v FS=\"(uniprotkb:|gene name)\" '{print $6, $9}' | sed -e 's/(//g' | tr ' ' '\\n' | sort -u | grep -v", bait, sep = " "), intern = TRUE)
                     incProgress(0.8)
                   })
      bait_interactors <- c(bait_inweb_im, bait_inweb3)
      bait_interactors <- unique(sort(bait_interactors))
    } else{
      return(NULL)
    }
  })
  
  #set sample size 
  #used for volcano and scatter plot
  sample <- reactive({
    if(!is.null(a_bait_friends())){
      bait_interactors <- a_bait_friends()
      pop <- population_bait()
      sample <- subset(pop, pop$gene %in% bait_interactors)
      rownames(sample) <- NULL
      sample
    }
  })
  
  sample_bait <- reactive({
    if(!is.null(a_bait_friends_vennd())){
      bait_interactors <- a_bait_friends_vennd()
      pop <- population_bait()
      sample <- subset(pop, pop$gene %in% bait_interactors)
      rownames(sample) <- NULL 
      sample
    }
  })
  
  sample_GOI <- reactive({
    if(!is.null(a_genes_uploaded_vennd())){
      genes_upload <- a_genes_uploaded_vennd()
      pop <- population_bait()
      sample <- lapply(genes_upload, function(x) subset(pop, gene %in% x) )  
      sample
    }
  })
  
  sample_SNP <- reactive({
    if(!is.null(SNP_to_gene_vennd())){
      snp2gene <- SNP_to_gene_vennd()
      pop <- population_GOI()
      sample <- subset(pop, pop$gene %in% snp2gene$gene)
      rownames(sample) <- NULL 
      sample
    }
  })
  
  sample_SNP_SGL <- reactive({
    if(!is.null(SNP_to_gene_vennd())){
      snp2gene <- SNP_to_gene_vennd()
      snp_sgl <- subset(snp2gene, Freq == 1)
      pop <- population_GOI()
      sample <- subset(pop, pop$gene %in% snp_sgl$gene)
      rownames(sample) <- NULL 
      sample
    }
  })
  
  sample_SNP_MGL <- reactive({
    if(!is.null(SNP_to_gene_vennd())){
      snp2gene <- SNP_to_gene_vennd()
      snp_mgl <- subset(snp2gene, Freq != 1)
      pop <- population_GOI()
      sample <- subset(pop, pop$gene %in% snp_mgl$gene)
      rownames(sample) <- NULL 
      sample
    }
  })
  
  success_sample_bait <- reactive({
    if(!is.null(sample_bait()) & !is.null(success_population_bait())){
      samp <- sample_bait()
      success_pop <- success_population_bait()
      success_samp <- subset(samp, samp$gene %in% success_pop$gene)
      rownames(success_samp) <- NULL
      success_samp
    }
  })
  
  success_sample_GOI <- reactive({
    if(!is.null(sample_GOI()) & !is.null(success_population_GOI())){
      samp <- sample_GOI()
      samp_total <- as.data.frame(data.table::rbindlist(samp))
      samp <- c(samp, list(total = samp_total))
      list_num <- input$a_goi_num_inputs
      success_pop <- success_population_GOI()
      success_samp <- subset(samp[[list_num]], samp[[list_num]]$gene %in% success_pop$gene)
      rownames(success_samp) <- NULL
      success_samp
    }
  })
  
  success_sample_SNP <- reactive({
    if(!is.null(sample_SNP()) & !is.null(success_population_GOI())){
      samp <- sample_SNP()
      success_pop <- success_population_GOI()
      success_samp <- subset(samp, samp$gene %in% success_pop$gene)
      rownames(success_samp) <- NULL
      success_samp
    }
  })
  
  success_sample_SNP_SGL <- reactive({
    if(!is.null(sample_SNP_SGL()) & !is.null(success_population_GOI())){
      samp <- sample_SNP_SGL()
      success_pop <- success_population_GOI()
      success_samp <- subset(samp, samp$gene %in% success_pop$gene)
      rownames(success_samp) <- NULL
      success_samp
    }
  })
  
  success_sample_SNP_MGL <- reactive({
    if(!is.null(sample_SNP_MGL()) & !is.null(success_population_GOI())){
      samp <- sample_SNP_MGL()
      success_pop <- success_population_GOI()
      success_samp <- subset(samp, samp$gene %in% success_pop$gene)
      rownames(success_samp) <- NULL
      success_samp
    }
  })
  
  hypergeometric_test_list <- reactive({
    if(!is.null(sample_bait()) & !is.null(success_population_bait()) & !is.null(success_sample_bait())){
      samp <- sample_bait()
      success_pop <- success_population_bait()
      success_samp <- success_sample_bait()
      bait <- a_bait_gene_vennd()
      all_interactor <- a_bait_friends_vennd()
      inweb_bait <- paste0("All known interactors of ", bait)
      x <- list()
      x[["A"]] <- as.character(success_pop$gene)
      x[["B"]] <- as.character(samp$gene)
      x[["Overlap"]] <- as.character(success_samp$gene)
      x[[inweb_bait]] <- as.character(all_interactor)
      n.obs <- sapply(x, length)
      seq.max <- seq_len(max(n.obs))
      mat <- sapply(x, "[", i = seq.max)
      mat[is.na(mat)] <- " "
      as.data.frame(mat)
    }
  })
  
  hypergeometric_test_list_GOI <- reactive({
    if(!is.null(sample_GOI()) & !is.null(success_population_GOI()) & !is.null(success_sample_GOI())){
      samp <- sample_GOI()
      samp_total <- as.data.frame(data.table::rbindlist(samp))
      samp <- c(samp, list(total = samp_total))
      list_num <- input$a_goi_num_inputs
      success_pop <- success_population_GOI()
      success_samp <- success_sample_GOI()
      genes_upload <- a_genes_uploaded_vennd()
      x <- list()
      x[["A"]] <- as.character(success_pop$gene)
      x[["B"]] <- as.character(samp[[list_num]]$gene)
      x[["Overlap"]] <- as.character(success_samp$gene)
      x[["Uploaded list of genes"]] <- as.character(genes_upload)
      n.obs <- sapply(x, length)
      seq.max <- seq_len(max(n.obs))
      mat <- sapply(x, "[", i = seq.max)
      mat[is.na(mat)] <- " "
      as.data.frame(mat)
    }
  })
  
  hypergeometric_test_list_SNP <- reactive({
    if(!is.null(sample_SNP()) & !is.null(success_population_SNP()) & !is.null(success_sample_SNP())){
      samp <- sample_SNP()
      success_pop <- success_population_SNP()
      success_samp <- success_sample_SNP()
      snp_upload <- a_snp_vennd()
      x <- list()
      x[["A"]] <- as.character(success_pop$gene)
      x[["B"]] <- as.character(samp$gene)
      x[["Overlap"]] <- as.character(success_samp$gene)
      n.obs <- sapply(x, length)
      seq.max <- seq_len(max(n.obs))
      mat <- sapply(x, "[", i = seq.max)
      mat[is.na(mat)] <- " "
      as.data.frame(mat)
    }
  })
  
  #snp to gene using LD r^2>0.6±50kb
  SNP_to_gene <- reactive({
    if(!is.null(a_snp())){
      withProgress(message = 'Finding genes in SNPs loci', 
                   detail = "Hold please", value = 0, {
        snp_data <- a_snp()
        incProgress(0.2)
        write.table(snp_data, file = "data/snp.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
        incProgress(0.6)
        system("scripts/genes_in_loci.sh")
        incProgress(0.8)
        SNPgeneList <- read.table("data/snp_to_gene.txt", header = TRUE)
        n_occur <- data.frame(table(SNPgeneList$snpid))
        incProgress(0.9)
        SNP_n_occur <- merge(SNPgeneList, n_occur, by.x = 'snpid', by.y = 'Var1')
      })
      snpList <- SNP_n_occur
    }
  })
  
  # eventReactive(input$a_make_vennd, 
  SNP_to_gene_vennd <- eventReactive(input$a_make_vennd_snp,{
    if(!is.null(a_snp_vennd())){
      withProgress(message = 'Finding genes in SNPs loci', 
                   detail = "Hold please", value = 0, {
        snp_data <- a_snp_vennd()
        incProgress(0.2)
        write.table(snp_data, file = "data/snp.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
        incProgress(0.6)
        system("scripts/genes_in_loci.sh")
        incProgress(0.8)
        SNPgeneList <- read.table("data/snp_to_gene.txt", header = TRUE)
        n_occur <- data.frame(table(SNPgeneList$snpid))
        incProgress(0.9)
        SNP_n_occur <- merge(SNPgeneList, n_occur, by.x = 'snpid', by.y = 'Var1')
      })
      snpList <- SNP_n_occur
    } else{
      return(NULL)
    }
  })
  
  a_pf_db <- reactive({
    if(!is.null(input$a_pfam_db)){
      pf_db <- input$a_pfam_db
    }
  })
  
  a_pf_db_search <- reactive({
    pf_db <- a_pf_db()
    selected_pf <- prot_fam[grep(paste(pf_db,collapse='|'), names(prot_fam))]
    selected_pf <- lapply(selected_pf, function(x) x[!is.na(x)])
    selected_pf
  })
  
  a_pf_cleanup <- reactive({
    validate(
      need(input$a_file_pulldown_r != '', ""),
      need(!is.null(a_pf_db()), "")
    )
    d <- a_orig_pulldown()
    d_col <- colnames(d)
    if("rep1" %in% d_col & "rep2" %in% d_col){
      prot_remove <- unique(unlist(a_pf_db_search()))
      prot_cleaned <- subset(d, gene %!in% prot_remove)
      prot_cleaned
    }
  })
  
  a_vp_colorbar <- reactive({
    FDR <- seq(0, 1, 0.01)
    limit <- rep("FDR", 101)
    d <- data.frame(limit, FDR)
    if(input$colorscheme == "fdr"){
      d1 <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = 0, xmax = 0.1, ymin = d1$FDR-0.01, ymax = d1$FDR)) + geom_rect(fill = mycol) +      
        scale_y_continuous(trans = "reverse", breaks = seq(0, 1, 0.1)) +
        labs(title = "FDR") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              panel.background=element_blank(),
              plot.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$colorscheme == "exac"){
      d1 <- separate_to_groups_for_exac_bar(d)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = 0, xmax = 0.1, ymin = d1$FDR-0.01, ymax = d1$FDR)) + geom_rect(fill = mycol) +
        labs(y = " pLI < 0.9     pLI >= 0.9    not in ExAC") +
        theme(axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$colorscheme == "cbf"){
      d1 <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = 0, xmax = 0.1, ymin = d1$FDR-0.01, ymax = d1$FDR)) + geom_rect(fill = mycol) +      
        scale_y_continuous(trans = "reverse", breaks = seq(0, 1, 0.1)) +
        labs(title = "FDR") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              panel.background=element_blank(),
              plot.title = element_text(size = rel(1))) + coord_fixed()
      bar
    }
  })
  
  a_vp_colorbar_dl <- reactive({
    FDR <- seq(0, 1, 0.01)
    limit <- rep("FDR", 101)
    d <- data.frame(limit, FDR)
    if(input$colorscheme == "fdr"){
      d1 <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = d1$FDR-0.01, xmax = d1$FDR, ymin = 0, ymax = 0.1)) + geom_rect(fill = mycol) +
        scale_x_continuous(breaks = seq(0, 1, 0.1)) +
        labs(x = "FDR") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$colorscheme == "exac"){
      d1 <- separate_to_groups_for_exac_bar(d)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = d1$FDR-0.01, xmax = d1$FDR, ymin = 0, ymax = 0.1)) + geom_rect(fill = mycol) +
        labs(x = " pLI < 0.9          pLI >= 0.9       not in ExAC") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$colorscheme == "cbf"){
      d1 <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = d1$FDR-0.01, xmax = d1$FDR, ymin = 0, ymax = 0.1)) + geom_rect(fill = mycol) +
        scale_x_continuous(breaks = seq(0, 1, 0.1)) +
        labs(x = "FDR") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    }
  })
  
  a_vp <- reactive({
    d <- a_pulldown()
    if(input$colorscheme == "fdr"){
      # data <- separate_to_groups_for_color(d, input$a_fdr_thresh)
      data <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      p <- plot_volcano_qc(data)
    } else if(input$colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_volcano_exac(below_thresh, above_thresh, no_exist)
    } else if(input$colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      p <- plot_volcano_qc(data)
      }
    p <- p %>%
      layout(xaxis = list(range=~c(min(logFC)-0.5, max(logFC)+0.5)),
             yaxis = list(range=~c(min(-log10(pvalue)-0.5), max(-log10(pvalue))+0.5))) %>% 
      add_lines(x = ~c(min(logFC)-0.5, max(logFC)+0.5), y = ~-log10(input$a_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$a_pval_thresh), showlegend = F) %>%
      add_lines(x = input$a_logFC_thresh, y = ~c(min(-log10(pvalue)-0.5), max(-log10(pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$a_logFC_thresh), y = ~c(min(-log10(pvalue)-0.5), max(-log10(pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  a_vp_plus_rep <- reactive({
    validate(
      need(!is.null(a_search_gene()), "")
    )
    p <- a_vp()
    goi <- a_search_gene()
    orig_data <- a_pulldown()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  a_pf_db_vp <- reactive({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    if(!is.null(a_pf_db_search())){
      d <- a_pulldown()
      gene_interest <- a_pf_db_search()
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )
      list(d_g2s=d_g2s)
    }
  })
  
  a_vp_pf_db <- reactive({
    validate(
      need(!is.null(a_pulldown()), ""),
      need(!is.null(a_pf_db()), "")
    )
    d <- a_pulldown()
    a_pf_db <- a_pf_db_vp()
    if(input$colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Purples", showlegend = T, width = 650, height = 550)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "Purples", showlegend = T, width = 650, height = 550)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      # p <- plot_volcano_exac(below_thresh, above_thresh, no_exist)
      p
    } else if(input$colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 650, height = 550)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    }
    p <- p %>%
      layout(xaxis = list(range=~c(min(logFC)-0.5, max(logFC)+0.5)),
             yaxis = list(range=~c(min(-log10(pvalue)-0.5), max(-log10(pvalue))+0.5)))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      df <- ldply(a_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$a_marker_text_prot_fam_db == "yes_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes(p, df)
        } else if(input$a_marker_text_prot_fam_db == "no_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes_no_text(p, df)
        }
        p <- vp_layer_genes
      } else{
        vp_layer_no_genes <- vp_layer_for_uploaded_genes_none(p, d)
        p <- vp_layer_no_genes
      }
    } else if(input$colorscheme == "cbf"){
      df <- ldply(a_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$a_marker_text_prot_fam_db == "yes_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes_cbf(p, df)
        } else if(input$a_marker_text_prot_fam_db == "no_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes_cbf_no_text(p, df)
        }
        p <- vp_layer_genes
      } else{
        vp_layer_no_genes <- vp_layer_for_uploaded_genes_none_cbf(p, d)
        p <- vp_layer_no_genes
      }
    }
    p <- p %>%
      add_lines(x = ~c(min(d$logFC)-0.5, max(d$logFC)+0.5), y = ~-log10(input$a_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$a_pval_thresh), showlegend = F) %>%
      add_lines(x = input$a_logFC_thresh, y = ~c(min(-log10(d$pvalue)-0.5), max(-log10(d$pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$a_logFC_thresh), y = ~c(min(-log10(d$pvalue)-0.5), max(-log10(d$pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  a_vp_count <- reactive({
    vp_data <- a_pulldown()
    FDR_pos <- nrow(subset(vp_data, (FDR < input$a_fdr_thresh) & (logFC > input$a_logFC_thresh)))
    FDR_neg <- nrow(subset(vp_data, (FDR < input$a_fdr_thresh) & (logFC < -(input$a_logFC_thresh))))
    pval_pos <- nrow(subset(vp_data, (pvalue < input$a_pval_thresh) & (logFC > input$a_logFC_thresh)))
    pval_neg <- nrow(subset(vp_data, (pvalue < input$a_pval_thresh) & (logFC < -(input$a_logFC_thresh))))
    total <- c(nrow(vp_data), nrow(vp_data))
    logFC_pos <- c(pval_pos, FDR_pos)
    logFC_neg <- c(pval_neg, FDR_neg)
    count <- data.frame(logFC_neg, logFC_pos, total)
    row.names(count) <- c(paste0("pvalue<", input$a_pval_thresh), paste0("FDR<", input$a_fdr_thresh))
    colnames(count) <- c(paste0("logFC<", -(input$a_logFC_thresh)), paste0("logFC>", input$a_logFC_thresh), "total")
    count
  })
  
  a_sp_cor <- reactive({
    input_file <- a_pulldown()
    if ("rep1" %in% colnames(input_file) & "rep2" %in% colnames(input_file)){
      cor <- signif(cor(input_file$rep1, input_file$rep2), 4)
      cor <- paste0("correlation coefficient: ", cor)
    } else if ("logFC" %in% colnames(input_file) & "FDR" %in% colnames(input_file) & "pvalue" %in% colnames(input_file)){
      return(NULL)
    }
  })
  
  a_sp <- reactive({
    d <- a_pulldown()
    cc <- a_sp_cor()
    if(input$colorscheme == "fdr"){
      d1 <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      p <- plot_scatter_qc(d, d1)
      p
    } else if(input$colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_scatter_exac(d, below_thresh, above_thresh, no_exist)
      p
    }
    if(input$colorscheme == "cbf"){
      d1 <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      p <- plot_scatter_qc(d, d1)
      p
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             title = cc, titlefont = list(size=15))
  })
  
  a_sp_plus <- reactive({
    validate(
      need(!is.null(a_search_gene()), "")
    )
    p <- a_sp()
    goi <- a_search_gene()
    orig_data <- a_pulldown()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_scatter(p, searchgene)
    p1
  })
  
  a_sp_pf_db <- reactive({
    validate(
      need(!is.null(a_pulldown()), ""),
      need(!is.null(a_pf_db()), "")
    )
    d <- a_pulldown()
    a_pf_db <- a_pf_db_vp()
    cc <- a_sp_cor()
    if(input$colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Purples", showlegend = F, width = 550, height = 550) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"),
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    } else if(input$colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "Purples", showlegend = F, width = 550, height = 550) 
      p <- add_lines(p, data = d, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      p <- add_markers(p, data = below_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p
    } else if(input$colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = F, width = 550, height = 550) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    }
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      df <- ldply(a_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$a_marker_text_prot_fam_db == "yes_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes(p, df)
        } else if(input$a_marker_text_prot_fam_db == "no_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_no_text(p, df)
        }
        p <- sp_layer_genes
      } else{
        sp_layer_no_genes <- sp_layer_for_uploaded_genes_none(p, d)
        p <- sp_layer_no_genes
      }
    } else if(input$colorscheme == "cbf"){
      df <- ldply(a_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$a_marker_text_prot_fam_db == "yes_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_cbf(p, df)
        } else if(input$a_marker_text_prot_fam_db == "no_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_cbf_no_text(p, df)
        }
        p <- sp_layer_genes
      } else{
        sp_layer_no_genes <- sp_layer_for_uploaded_genes_none_cbf(p, d)
        p <- sp_layer_no_genes
      }
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(d$rep1, d$rep2))-1, (max(d$rep1, d$rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(d$rep1, d$rep2))-1, (max(d$rep1, d$rep2))+1)), 
             title = cc, titlefont = list(size=12))
  })
  
  a_multi_vp <- eventReactive(input$a_make_plot, {
    validate(
      need(!is.null(a_pulldown()), "")
    )
    d <- a_pulldown()

    # InWeb, SNP to gene, and genes upload
    if(!is.null(a_bait_gene_layer()) & !is.null(a_snp()) & !is.null(a_upload_genes())){
      inwebFile <- sample()
      gene_interest <- a_genes_uploaded()
      snp_interest <- SNP_to_gene()
      bait <- a_bait_gene_layer()
      d_in <- subset(d, gene %in% inwebFile$gene & gene != bait)
      d_snp <- merge(d, snp_interest, by.x = 'gene', by.y = 'gene')
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )  
      list(d_in=d_in, d_snp=d_snp, d_g2s=d_g2s)
    } else if(!is.null(a_bait_gene_layer()) & is.null(a_snp()) & is.null(a_upload_genes())){
      # InWeb only 
      inwebFile <- sample()
      bait <- a_bait_gene_layer()
      d_in <- subset(d, gene %in% inwebFile$gene & gene != bait)
      list(d_in=d_in)
    } else if(is.null(a_bait_gene_layer()) & is.null(a_snp()) & is.null(a_upload_genes())){
      # None
    } else if(!is.null(a_bait_gene_layer()) & is.null(a_snp()) & !is.null(a_upload_genes())){
      # InWeb, and genes upload
      inwebFile <- sample()
      gene_interest <- a_genes_uploaded()
      bait <- a_bait_gene_layer()
      d_in <- subset(d, gene %in% inwebFile$gene & gene != bait)
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )  
      list(d_in=d_in, d_g2s=d_g2s)
    } else if(is.null(a_bait_gene_layer()) & !is.null(a_snp()) & !is.null(a_upload_genes())){
      # SNP to gene, and genes upload
      gene_interest <- a_genes_uploaded()
      snp_interest <- SNP_to_gene()
      d_snp <- merge(d, snp_interest, by.x = 'gene', by.y = 'gene')
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )
      list(d_snp=d_snp, d_g2s=d_g2s)
    } else if(is.null(a_bait_gene_layer()) & is.null(a_snp()) & !is.null(a_upload_genes())){
      # Genes upload only
      gene_interest <- a_genes_uploaded()
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )  
      list(d_g2s=d_g2s)
    } else if(is.null(a_bait_gene_layer()) & !is.null(a_snp()) & is.null(a_upload_genes())){
      # SNP to gene only
      snp_interest <- SNP_to_gene()
      d_snp <- merge(d, snp_interest, by.x = 'gene', by.y = 'gene')
      list(d_snp=d_snp)
    } else if(!is.null(a_bait_gene_layer()) & !is.null(a_snp()) & is.null(a_upload_genes())){
      # InWeb, and SNP to gene
      inwebFile <- sample()
      snp_interest <- SNP_to_gene()
      bait <- a_bait_gene_layer()
      d_in <- subset(d, gene %in% inwebFile$gene & gene != bait)
      d_snp <- merge(d, snp_interest, by.x = 'gene', by.y = 'gene')
      list(d_in=d_in, d_snp=d_snp)
    }
  })
  
  a_multi_vp_colorbar <- reactive({
    #wait until multi_vp is created
    multi_vp <- a_multi_vp()
    FDR <- seq(0, 1, 0.01)
    limit <- rep("FDR", 101)
    d <- data.frame(limit, FDR)
    if(input$colorscheme == "fdr"){
      d1 <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = 0, xmax = 0.1, ymin = d1$FDR-0.01, ymax = d1$FDR)) + geom_rect(fill = mycol) +      
        scale_y_continuous(trans = "reverse", breaks = seq(0, 1, 0.1)) +
        labs(title = "FDR") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              panel.background=element_blank(),
              plot.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$colorscheme == "exac"){
      d1 <- separate_to_groups_for_exac_bar(d)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = 0, xmax = 0.1, ymin = d1$FDR-0.01, ymax = d1$FDR)) + geom_rect(fill = mycol) +
        labs(y = " pLI < 0.9     pLI >= 0.9    not in ExAC") +
        theme(axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$colorscheme == "cbf"){
      d1 <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = 0, xmax = 0.1, ymin = d1$FDR-0.01, ymax = d1$FDR)) + geom_rect(fill = mycol) +      
        scale_y_continuous(trans = "reverse", breaks = seq(0, 1, 0.1)) +
        labs(title = "FDR") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              panel.background=element_blank(),
              plot.title = element_text(size = rel(1))) + coord_fixed()
      bar
    }
  })
  
  a_multi_vp_colorbar_dl <- reactive({
    FDR <- seq(0, 1, 0.01)
    limit <- rep("FDR", 101)
    d <- data.frame(limit, FDR)
    if(input$colorscheme == "fdr"){
      d1 <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = d1$FDR-0.01, xmax = d1$FDR, ymin = 0, ymax = 0.1)) + geom_rect(fill = mycol) +
        scale_x_continuous(breaks = seq(0, 1, 0.1)) +
        labs(x = "FDR") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$colorscheme == "exac"){
      d1 <- separate_to_groups_for_exac_bar(d)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = d1$FDR-0.01, xmax = d1$FDR, ymin = 0, ymax = 0.1)) + geom_rect(fill = mycol) +
        labs(x = " pLI < 0.9          pLI >= 0.9       not in ExAC") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$colorscheme == "cbf"){
      d1 <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = d1$FDR-0.01, xmax = d1$FDR, ymin = 0, ymax = 0.1)) + geom_rect(fill = mycol) +
        scale_x_continuous(breaks = seq(0, 1, 0.1)) +
        labs(x = "FDR") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    }
  })

  a_multi_vp_layer <- reactive({
    multi_vp <- a_multi_vp()
    d <- a_pulldown()
    if(input$colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 650, height = 550)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 650, height = 550)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p
    } else if(input$colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 650, height = 550)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p %>%
      layout(xaxis = list(range=~c(min(logFC)-0.5, max(logFC)+0.5)),
             yaxis = list(range=~c(min(-log10(pvalue)-0.5), max(-log10(pvalue))+0.5)))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      # InWeb, SNP to gene, and genes upload
      if(!is.null(a_bait_gene_layer())){
        if(input$a_marker_text == "yes_label"){
          vp_layer_inweb <- vp_layer_for_inweb(p, multi_vp$d_in)
        } else if(input$a_marker_text == "no_label"){
          vp_layer_inweb <- vp_layer_for_inweb_no_text(p, multi_vp$d_in)
        }
        p <- vp_layer_inweb
      }
      if(!is.null(a_snp())){
        if(nrow(multi_vp$d_snp) != 0){
          snp_sgl <- subset(multi_vp$d_snp, Freq == 1)
          snp_mgl <- subset(multi_vp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$a_marker_text == "yes_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl(p, snp_sgl)
            } else if(input$a_marker_text == "no_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_no_text(p, snp_sgl)
            }
            p <- vp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$a_marker_text == "yes_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl(p, snp_mgl)
            } else if(input$a_marker_text == "no_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_no_text(p, snp_mgl)
            }
            p <- vp_layer_snp2gene_mgl
          }
        } else{
          vp_layer_no_snp2gene <- vp_layer_for_snp_to_gene_none(p, d)
          p <- vp_layer_no_snp2gene
        }
      }
      if(!is.null(a_upload_genes())){
        df <- ldply(multi_vp$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$a_marker_text == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes(p, df)
          } else if(input$a_marker_text == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none(p, d)
          p <- vp_layer_no_genes
        }
      }
      p
    } else if(input$colorscheme == "cbf"){
      # InWeb, SNP to gene, and genes upload
      if(!is.null(a_bait_gene_layer())){
        if(input$a_marker_text == "yes_label"){
          vp_layer_inweb <- vp_layer_for_inweb_cbf(p, multi_vp$d_in)
        } else if(input$a_marker_text == "no_label"){
          vp_layer_inweb <- vp_layer_for_inweb_cbf_no_text(p, multi_vp$d_in)
        }
        p <- vp_layer_inweb
      }
      if(!is.null(a_snp())){
        if(nrow(multi_vp$d_snp) != 0){
          snp_sgl <- subset(multi_vp$d_snp, Freq == 1)
          snp_mgl <- subset(multi_vp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$a_marker_text == "yes_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_cbf(p, snp_sgl)
            } else if(input$a_marker_text == "no_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_cbf_no_text(p, snp_sgl)
            }
            p <- vp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$a_marker_text == "yes_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_cbf(p, snp_mgl)
            } else if(input$a_marker_text == "no_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_cbf_no_text(p, snp_mgl)
            }
            p <- vp_layer_snp2gene_mgl
          }
        } else{
          vp_layer_no_snp2gene <- vp_layer_for_snp_to_gene_none_cbf(p, d)
          p <- vp_layer_no_snp2gene
        }
      }
      if(!is.null(a_upload_genes())){
        df <- ldply(multi_vp$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$a_marker_text == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf(p, df)
          } else if(input$a_marker_text == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none_cbf(p, d)
          p <- vp_layer_no_genes
        }
      }
      p
    }
    p <- p %>% 
      add_lines(x = c(min(d$logFC)-0.5, max(d$logFC)+0.5), y = ~-log10(input$a_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$a_pval_thresh), showlegend = F) %>%
      add_lines(x = input$a_logFC_thresh, y = c(min(-log10(d$pvalue)-0.5), max(-log10(d$pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$a_logFC_thresh), y = c(min(-log10(d$pvalue)-0.5), max(-log10(d$pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
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
  
  a_multi_vp_count <- reactive({
    multi_vp <- a_multi_vp()
    vp_data <- a_pulldown()
    FDR_pos <- nrow(subset(vp_data, (FDR < input$a_fdr_thresh) & (logFC > input$a_logFC_thresh)))
    FDR_neg <- nrow(subset(vp_data, (FDR < input$a_fdr_thresh) & (logFC < -(input$a_logFC_thresh))))
    pval_pos <- nrow(subset(vp_data, (pvalue < input$a_pval_thresh) & (logFC > input$a_logFC_thresh)))
    pval_neg <- nrow(subset(vp_data, (pvalue < input$a_pval_thresh) & (logFC < -(input$a_logFC_thresh))))
    logFC_pos <- c(pval_pos, FDR_pos)
    logFC_neg <- c(pval_neg, FDR_neg)
    orig_count <- data.frame(logFC_neg, logFC_pos)
    row.names(orig_count) <- c(paste0("pvalue<", input$a_pval_thresh), paste0("FDR<", input$a_fdr_thresh))
    colnames(orig_count) <- c(paste0("logFC<", -(input$a_logFC_thresh)), paste0("logFC>", input$a_logFC_thresh))
    
    temprow <- matrix(c(rep.int("",length(orig_count))),nrow=1,ncol=length(orig_count))
    newrow <- data.frame(temprow)
    colnames(newrow) <- colnames(orig_count)
    row.names(newrow) <- ""
    temprow1 <- matrix(c("enriched-left", "enriched-right"),nrow=1,ncol=length(orig_count))
    newrow1 <- data.frame(temprow1)
    colnames(newrow1) <- colnames(orig_count)
    row.names(newrow1) <- " "
    orig_count <- rbind(orig_count, newrow)
    orig_count <- rbind(orig_count, newrow1)
    
    # InWeb, SNP to gene, and genes upload
    if(!is.null(a_bait_gene_layer())){
      if(nrow(multi_vp$d_in)>0){
        inweb_pos <- nrow(subset(multi_vp$d_in, (FDR < input$a_fdr_thresh) & (logFC > input$a_logFC_thresh) & (pvalue < input$a_pval_thresh)))
        inweb_neg <- nrow(subset(multi_vp$d_in, (FDR < input$a_fdr_thresh) & (logFC < -(input$a_logFC_thresh)) & (pvalue < input$a_pval_thresh)))
        i_count <- data.frame(inweb_neg, inweb_pos) #, nrow(multi_vp$d_in)
        row.names(i_count) <- c("InWeb")
        colnames(i_count) <- c(paste0("logFC<", -(input$a_logFC_thresh)), paste0("logFC>", input$a_logFC_thresh))
        orig_count <- do.call("rbind", list(orig_count, i_count))
      } else {
        orig_count
      }
    }
    
    if(!is.null(a_snp())){
      if(nrow(multi_vp$d_snp)>0){
        snp_pos <- nrow(subset(multi_vp$d_snp, (FDR < input$a_fdr_thresh) & (logFC > input$a_logFC_thresh) & (pvalue < input$a_pval_thresh)))
        snp_neg <- nrow(subset(multi_vp$d_snp, (FDR < input$a_fdr_thresh) & (logFC < -(input$a_logFC_thresh)) & (pvalue < input$a_pval_thresh)))
        s_count <- data.frame(snp_neg, snp_pos) #, nrow(multi_vp$d_snp)
        row.names(s_count) <- c("SNP")
        colnames(s_count) <- c(paste0("logFC<", -(input$a_logFC_thresh)), paste0("logFC>", input$a_logFC_thresh))
        orig_count <- do.call("rbind", list(orig_count, s_count))
      } else {
        orig_count
      }
    }
    
    if(!is.null(a_upload_genes())){
      if(length(multi_vp$d_g2s)>0){
        df <- ldply(multi_vp$d_g2s, data.frame)
        df1 <- split(df, df$.id)
        gene_pos <- lapply(df1, function(x) subset(x, (FDR < input$a_fdr_thresh) & (logFC > input$a_logFC_thresh) & (pvalue < input$a_pval_thresh)))  
        gene_neg <- lapply(df1, function(x) subset(x, (FDR < input$a_fdr_thresh) & (logFC < -(input$a_logFC_thresh)) & (pvalue < input$a_pval_thresh)))  
        gene_pos1 <- lapply(gene_pos, function(x) nrow(x))
        gene_neg1 <- lapply(gene_neg, function(x) nrow(x))
        df1_count <- lapply(df1, function(x) nrow(x))
        pos_count <- ldply(gene_pos1, data.frame)
        neg_count <- ldply(gene_neg1, data.frame)
        df1_total <- ldply(df1_count, data.frame)
        g_count <- data.frame(neg_count$X..i.., pos_count$X..i..) #, df1_total$X..i..
        row.names(g_count) <- df1_total$.id
        colnames(g_count) <- c(paste0("logFC<", -(input$a_logFC_thresh)), paste0("logFC>", input$a_logFC_thresh))
        orig_count <- do.call("rbind", list(orig_count, g_count))
      } else {
        orig_count
      }
    }
    orig_count
  })
  
  a_multi_vp_count_text <- reactive({
    if(!is.null(a_multi_vp_count())){
    enriched1 <- c(paste0("enriched-left: pvalue<", input$a_pval_thresh, ", FDR<", input$a_fdr_thresh, ", logFC<", -(input$a_logFC_thresh)))
    enriched2 <- c(paste0("enriched-right: pvalue<", input$a_pval_thresh, ", FDR<", input$a_fdr_thresh, ", logFC>", input$a_logFC_thresh))
    list(a=enriched1, b=enriched2)
    }
  })
  
  a_multi_sp_layer <- reactive({
    multi_sp <- a_multi_vp()
    d <- a_pulldown()
    cc <- a_sp_cor()
    if(input$colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 650, height = 550) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"),
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    } else if(input$colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 650, height = 550) 
      p <- add_lines(p, data = d, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      p <- add_markers(p, data = below_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      # p <- plot_scatter_exac(d, below_thresh, above_thresh, no_exist)
      p
    } else if(input$colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 650, height = 550) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             title = cc, titlefont = list(size=15))

    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      # InWeb, SNP to gene, and genes upload
      if(!is.null(a_bait_gene_layer())){
        if(input$a_marker_text == "yes_label"){
          sp_layer_inweb <- sp_layer_for_inweb(p, multi_sp$d_in)
        } else if(input$a_marker_text == "no_label"){
          sp_layer_inweb <- sp_layer_for_inweb_no_text(p, multi_sp$d_in)
        }
        p <- sp_layer_inweb
      }
      if(!is.null(a_snp())){
        if(nrow(multi_sp$d_snp) != 0){
          snp_sgl <- subset(multi_sp$d_snp, Freq == 1)
          snp_mgl <- subset(multi_sp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$a_marker_text == "yes_label"){
              sp_layer_snp2gene_sgl <- sp_layer_for_snp_to_gene_sgl(p, snp_sgl)
            } else if(input$a_marker_text == "no_label"){
              sp_layer_snp2gene_sgl <- sp_layer_for_snp_to_gene_sgl_no_text(p, snp_sgl)
            }
            p <- sp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$a_marker_text == "yes_label"){
              sp_layer_snp2gene_mgl <- sp_layer_for_snp_to_gene_mgl(p, snp_mgl)
            } else if(input$a_marker_text == "no_label"){
              sp_layer_snp2gene_mgl <- sp_layer_for_snp_to_gene_mgl_no_text(p, snp_mgl)
            }
            p <- sp_layer_snp2gene_mgl
          }
        } else{
          sp_layer_no_snp2gene <- sp_layer_for_snp_to_gene_none(p, d)
          p <- sp_layer_no_snp2gene
        }
      }
      if(!is.null(a_upload_genes())){
        df <- ldply(multi_sp$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$a_marker_text == "yes_label"){
            sp_layer_genes <- sp_layer_for_uploaded_genes(p, df)
          } else if(input$a_marker_text == "no_label"){
            sp_layer_genes <- sp_layer_for_uploaded_genes_no_text(p, df)
          }
          p <- sp_layer_genes
        } else{
          sp_layer_no_genes <- sp_layer_for_uploaded_genes_none(p, d)
          p <- sp_layer_no_genes
        }
      }
      p
    } else if(input$colorscheme == "cbf"){
      # InWeb, SNP to gene, and genes upload
      if(!is.null(a_bait_gene_layer())){
        if(input$a_marker_text == "yes_label"){
          sp_layer_inweb <- sp_layer_for_inweb_cbf(p, multi_sp$d_in)
        } else if(input$a_marker_text == "no_label"){
          sp_layer_inweb <- sp_layer_for_inweb_cbf_no_text(p, multi_sp$d_in)
        }
        p <- sp_layer_inweb
      }
      if(!is.null(a_snp())){
        if(nrow(multi_sp$d_snp) != 0){
          snp_sgl <- subset(multi_sp$d_snp, Freq == 1)
          snp_mgl <- subset(multi_sp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$a_marker_text == "yes_label"){
              sp_layer_snp2gene_sgl <- sp_layer_for_snp_to_gene_sgl_cbf(p, snp_sgl)
            } else if(input$a_marker_text == "no_label"){
              sp_layer_snp2gene_sgl <- sp_layer_for_snp_to_gene_sgl_cbf_no_text(p, snp_sgl)
            }
            p <- sp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$a_marker_text == "yes_label"){
              sp_layer_snp2gene_mgl <- sp_layer_for_snp_to_gene_mgl_cbf(p, snp_mgl)
            } else if(input$a_marker_text == "no_label"){
              sp_layer_snp2gene_mgl <- sp_layer_for_snp_to_gene_mgl_cbf_no_text(p, snp_mgl)
            }
            p <- sp_layer_snp2gene_mgl
          }
        } else{
          sp_layer_no_snp2gene <- sp_layer_for_snp_to_gene_none_cbf(p, d)
          p <- sp_layer_no_snp2gene
        }
      }
      if(!is.null(a_upload_genes())){
        df <- ldply(multi_sp$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$a_marker_text == "yes_label"){
            sp_layer_genes <- sp_layer_for_uploaded_genes_cbf(p, df)
          } else if(input$a_marker_text == "no_label"){
            sp_layer_genes <- sp_layer_for_uploaded_genes_cbf_no_text(p, df)
          }
          p <- sp_layer_genes
        } else{
          sp_layer_no_genes <- sp_layer_for_uploaded_genes_none_cbf(p, d)
          p <- sp_layer_no_genes
        }
      }
      p
    }
    p
  })
  
  a_multi_sp_plus <- reactive({
    validate(
      need(!is.null(a_search_gene()), "")
    )
    p <- a_multi_sp_layer()
    goi <- a_search_gene()
    orig_data <- a_pulldown()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_scatter(p, searchgene)
    p1
  })
  
  output$a_VD_sig_text <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    HTML("<b>User-defined significance thresholds:</b>")
  })
  
  a_venndiagram <- reactive({
    bait <- a_bait_gene_vennd()
    success_pop <- success_population_bait()
    s_p <- unique(sort(success_pop$gene))
    pop <- population_bait()
    p <- unique(sort(pop$gene))
    samp <- sample_bait()
    s <- unique(sort(samp$gene))
    success_samp <- success_sample_bait()
    s_s <- unique(sort(success_samp$gene))
    success_pop_l <- length(s_p)
    pop_l <- length(p)
    samp_l <- length(s)
    success_samp_l <- length(s_s)
    pvalue <- phyper((success_samp_l-1), samp_l, (pop_l-samp_l), success_pop_l, lower.tail = F)
    mycolours3 = c("cornflowerblue", "yellow1")
    
    x <- list()
    x[["A"]] <- success_pop$gene
    x[["B"]] <- samp$gene
    
    v0 <- venn.diagram(x,
                       col = mycolours3, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                       main = paste("p value = ", format(pvalue, digits = 4)),
                       sub = " ", sub.pos = c(0, 0), scaled = F,
                       cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05),
                       fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
    
    v0
  })
  
  a_venndiagram_text <- reactive({
    bait <- a_bait_gene_vennd()
    pop <- population_bait()
    success_pop <- success_population_bait()
    samp <- sample_bait()
    subset_limit <- paste0("A = pull down subset of ", input$a_FDR_range[1], "&lt;FDR&lt;", input$a_FDR_range[2], 
                           ", ", input$a_logFC_range[1], "&lt;logFC&lt;", input$a_logFC_range[2], 
                           " and ", input$a_pvalue_range[1], "&lt;pvalue&lt;", input$a_pvalue_range[2],
                           " &#40;", length(unique(sort(success_pop$gene))), "&#41;")
    inweb_name <- paste0("B = InWeb_IM interactors of ", bait, " &#40;", nrow(samp), "&#41;")
    total <- paste0("pull down &cap; InWeb_IM database &#40;", nrow(pop), "&#41;")
    list(a=subset_limit, b=inweb_name, c=total)
  })
  
  output$a_vd_text <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', ""),
      need(input$a_bait_vennd != '', "")
    )
    output <- a_venndiagram_text()
    HTML(paste(output$c, output$a, output$b, sep = "<br/>"))
  })
  
  a_venndiagram_GOI <- reactive({
    success_pop <- success_population_GOI()
    s_p <- unique(sort(success_pop$gene))
    pop <- population_GOI()
    p <- unique(sort(pop$gene))
    samp <- sample_GOI()
    samp_total <- as.data.frame(data.table::rbindlist(samp))
    samp <- c(samp, list(total = samp_total))
    list_num <- input$a_goi_num_inputs
    s <- unique(sort(samp[[list_num]]$gene))
    success_samp <- success_sample_GOI()
    s_s <- unique(sort(success_samp$gene))
    success_pop_l <- length(s_p)
    pop_l <- length(p)
    samp_l <- length(s)
    success_samp_l <- length(s_s)
    pvalue <- phyper((success_samp_l-1), samp_l, (pop_l-samp_l), success_pop_l, lower.tail = F)
    mycolours3 = c("cornflowerblue", "#fdb462")
    
    x <- list()
    x[["A"]] <- success_pop$gene
    x[["B"]] <- samp[[list_num]]$gene
    
    v0 <- venn.diagram(x, 
                       col = mycolours3, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                       main = paste("p value = ", format(pvalue, digits = 4)), 
                       sub = " ", sub.pos = c(0, 0), scaled = F,
                       cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05))
    for (i in 1:length(v0)){ v0[[i]]$gp$fontfamily <- 'Arial' }
    
    v0
  })
  
  a_venndiagram_GOI_text <- reactive({ 
    pop <- population_GOI()
    success_pop <- success_population_GOI()
    samp <- sample_GOI()
    samp_total <- as.data.frame(data.table::rbindlist(samp))
    samp <- c(samp, list(total = samp_total))
    list_num <- input$a_goi_num_inputs
    subset_limit <- paste0("A = pull down subset of ", input$a_FDR_range[1], "&lt;FDR&lt;", input$a_FDR_range[2], 
                           ", ", input$a_logFC_range[1], "&lt;logFC&lt;", input$a_logFC_range[2], 
                           " and ", input$a_pvalue_range[1], "&lt;pvalue&lt;", input$a_pvalue_range[2],
                           " &#40;", length(unique(sort(success_pop$gene))), "&#41;")
    inweb_name <- paste0("B = Genes of interest in HGNC database", " &#40;", nrow(samp[[list_num]]), "&#41;")
    total <- paste0("pull down &cap; HGNC database &#40;", nrow(pop), "&#41;")
    list(a=subset_limit, b=inweb_name, c=total)
  })
  
  output$a_vd_GOI_text <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', ""),
      need(!is.null(input$a_file_genes_vennd), "")
    )
    output <- a_venndiagram_GOI_text()
    HTML(paste(output$c, output$a, output$b, sep = "<br/>"))
  })
  
  a_venndiagram_SNP <- reactive({
    success_pop <- success_population_GOI()
    s_p <- unique(sort(success_pop$gene))
    pop <- population_GOI()
    p <- unique(sort(pop$gene))
    samp <- sample_SNP()
    s <- unique(sort(samp$gene))
    success_samp <- success_sample_SNP()
    s_s <- unique(sort(success_samp$gene))
    success_pop_l <- length(s_p)
    pop_l <- length(p)
    samp_l <- length(s)
    success_samp_l <- length(s_s)
    mycolours3 = c("cornflowerblue", "salmon")
    
    x <- list()
    x[["A"]] <- success_pop$gene
    x[["B"]] <- samp$gene
    
    v0 <- venn.diagram(x, 
                       col = mycolours3, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                       main = "all SNPs", 
                       sub = " ", sub.pos = c(0, 0), scaled = F,
                       cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05))
    for (i in 1:length(v0)){ v0[[i]]$gp$fontfamily <- 'Arial' }
    v0
  })
  
  a_venndiagram_SNP_text <- reactive({
    pop <- population_GOI()
    success_pop <- success_population_GOI()
    samp <- sample_SNP()
    subset_limit <- paste0("A = pull down subset of ", input$a_FDR_range[1], "&lt;FDR&lt;", input$a_FDR_range[2], 
                           ", ", input$a_logFC_range[1], "&lt;logFC&lt;", input$a_logFC_range[2], 
                           " and ", input$a_pvalue_range[1], "&lt;pvalue&lt;", input$a_pvalue_range[2],
                           " &#40;", length(unique(sort(success_pop$gene))), "&#41;")
    inweb_name <- paste0("B = SNPs in 1K Genome database", " &#40;", nrow(samp), "&#41;")
    total <- paste0("pull down &cap; HGNC database &#40;", nrow(pop), "&#41;")
    list(a=subset_limit, b=inweb_name, c=total)
  })
  
  output$a_vd_SNP_text <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', ""),
      need(!is.null(input$a_file_SNP_vennd), "")
    )
    output <- a_venndiagram_SNP_text()
    HTML(paste(output$a, output$b, sep = "<br/>"))
  })
  
  a_venndiagram_SNP_SGL <- reactive({
    success_pop <- success_population_GOI()
    s_p <- unique(sort(success_pop$gene))
    pop <- population_GOI()
    p <- unique(sort(pop$gene))
    samp <- sample_SNP_SGL()
    s <- unique(sort(samp$gene))
    success_samp <- success_sample_SNP_SGL()
    s_s <- unique(sort(success_samp$gene))
    success_pop_l <- length(s_p)
    pop_l <- length(p)
    samp_l <- length(s)
    success_samp_l <- length(s_s)
    mycolours3 = c("cornflowerblue", "#80cdc1")

    x <- list()
    x[["A"]] <- success_pop$gene
    x[["B"]] <- samp$gene
    
    v0 <- venn.diagram(x, 
                       col = mycolours3, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                       main = "Single-gene-loci SNPs", 
                       sub = " ", sub.pos = c(0, 0), scaled = F,
                       cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05))
    for (i in 1:length(v0)){ v0[[i]]$gp$fontfamily <- 'Arial' }
    v0
  })
  
  a_venndiagram_SNP_SGL_text <- reactive({
    pop <- population_GOI()
    success_pop <- success_population_GOI()
    samp <- sample_SNP_SGL()
    subset_limit <- paste0("A = pull down subset of ", input$a_FDR_range[1], "&lt;FDR&lt;", input$a_FDR_range[2], 
                           ", ", input$a_logFC_range[1], "&lt;logFC&lt;", input$a_logFC_range[2], 
                           " and ", input$a_pvalue_range[1], "&lt;pvalue&lt;", input$a_pvalue_range[2],
                           " &#40;", length(unique(sort(success_pop$gene))), "&#41;")
    inweb_name <- paste0("B = SGL SNPs in 1K Genome database", " &#40;", nrow(samp), "&#41;")
    total <- paste0("pull down &cap; HGNC database &#40;", nrow(pop), "&#41;")
    list(a=subset_limit, b=inweb_name, c=total)
  })
  
  output$a_vd_SNP_SGL_text <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', ""),
      need(!is.null(input$a_file_SNP_vennd), "")
    )
    output <- a_venndiagram_SNP_SGL_text()
    HTML(paste(output$a, output$b, sep = "<br/>"))
  })
  
  a_venndiagram_SNP_MGL <- reactive({ 
    success_pop <- success_population_GOI()
    s_p <- unique(sort(success_pop$gene))
    pop <- population_GOI()
    p <- unique(sort(pop$gene))
    samp <- sample_SNP_MGL()
    s <- unique(sort(samp$gene))
    success_samp <- success_sample_SNP_MGL()
    s_s <- unique(sort(success_samp$gene))
    success_pop_l <- length(s_p)
    pop_l <- length(p)
    samp_l <- length(s)
    success_samp_l <- length(s_s)
    
    x <- list()
    x[["A"]] <- success_pop$gene
    x[["B"]] <- samp$gene
    
    v0 <- venn.diagram(x, 
                       col = mycolours3, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                       main = "Multi-genes-loci SNPs", 
                       sub = " ", sub.pos = c(0, 0), scaled = F,
                       cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05))
    for (i in 1:length(v0)){ v0[[i]]$gp$fontfamily <- 'Arial' }
    v0
  })
  
  a_venndiagram_SNP_MGL_text <- reactive({
    pop <- population_GOI()
    success_pop <- success_population_GOI()
    samp <- sample_SNP_MGL()
    subset_limit <- paste0("A = pull down subset of ", input$a_FDR_range[1], "&lt;FDR&lt;", input$a_FDR_range[2], 
                           ", ", input$a_logFC_range[1], "&lt;logFC&lt;", input$a_logFC_range[2], 
                           " and ", input$a_pvalue_range[1], "&lt;pvalue&lt;", input$a_pvalue_range[2],
                           " &#40;", length(unique(sort(success_pop$gene))), "&#41;")
    inweb_name <- paste0("B = MGL SNPs in 1K Genome database", " &#40;", nrow(samp), "&#41;")
    total <- paste0("pull down &cap; HGNC database &#40;", nrow(pop), "&#41;")
    list(a=subset_limit, b=inweb_name, c=total)
  })
  
  output$a_vd_SNP_MGL_text <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', ""),
      need(!is.null(input$a_file_SNP_vennd), "")
    )
    output <- a_venndiagram_SNP_MGL_text()
    HTML(paste(output$a, output$b, sep = "<br/>"))
  })
  
  a_BPF_marker <- reactive({
    inBPFmarker <- input$a_BPF_option
    if (inBPFmarker == "change_m_BPF") {
      marker <- "change"
    } else {
      marker <- "no_change"
    }
  })
  
  a_bpf_data <- eventReactive(input$a_make_bpf, {
    validate(
      need(input$a_file_pulldown_r != '', "Upload file")
    )
    withProgress(message = 'This may take a while', 
                 detail = 'Hold please', value = 0, {
                   bpf <- a_pulldown()
                   
                   makePlotFamilies_1quadrant <- function(data, bait){
                     im <- subset(data, 
                                  (pvalue < input$a_BPF_pvalue_range[2] & pvalue > input$a_BPF_pvalue_range[1]) & 
                                    (FDR < input$a_BPF_FDR_range[2] & FDR > input$a_BPF_FDR_range[1]) &
                                    (logFC < input$a_BPF_logFC_range[2] & logFC > input$a_BPF_logFC_range[1]))
                     incProgress(0.6)
                     enM_families <- assignFamily_inc_doubles(im)
                     enM_gna <- addNames(im)
                     incProgress(0.8)
                     ### replace logFC and pvalue in di* by these from dpm
                     enM_families$logFC <- data$logFC[match(enM_families$gene, data$gene)]
                     enM_families$pvalue <- data$pvalue[match(enM_families$gene, data$gene)]
                     enM_gna$logFC <- data$logFC[match(enM_gna$gene, data$gene)]
                     enM_gna$pvalue <- data$pvalue[match(enM_gna$gene, data$gene)]
                     
                     # mybait <- data[grepl(bait,data$gene),]
                     
                     howmany <- length(unique(enM_families$family))
                     bpfmsizing <- a_BPF_marker()
                     increase_size <- input$a_BPF_marker_freq
                     
                     incProgress(0.9)
                     bpf_list <- list("list" = bpf, "list" = enM_families, "list" = enM_gna, "integer" = howmany, bpfmsizing, increase_size)
                     return(bpf_list)
                   }
                   makePlotFamilies_1quadrant(bpf)
                 })
  }) 
  
  a_bpf_families <- reactive({
    bpf_data <- a_bpf_data()
    enM_families <- bpf_data[[2]]
    families <- cbind(enM_families$gene, enM_families$family, enM_families$frequency)
    colnames(families) <- c("gene", "family", "frequency")
    families
  })
  
  a_bpf <- reactive({
    bpf_data <- a_bpf_data()
    bpf <- bpf_data[[1]]
    enM_families <- bpf_data[[2]]
    enM_gna <- bpf_data[[3]]
    howmany <- bpf_data[[4]]
    bpfmsizing <- bpf_data[[5]]
    increase_size <- bpf_data[[6]]
    
    if (bpfmsizing == "change"){
      p <- plot_ly(colors = rainbow(howmany), width = 1200, height = 800) %>%
        #background
        add_markers(data = bpf, x = ~logFC, y = ~-log10(pvalue), opacity = 0.6,
                    marker = list(color = 'rgba(176,196,222,08)'),
                    text = ~paste(gene), hoverinfo = "text", showlegend = FALSE) %>%
        add_markers(data = enM_gna, x = ~logFC, y = ~-log10(pvalue), 
                    marker = list(size = 6, symbol = 2, color = c('white'), opacity = 0.8, line = list(width=0.9, color = "black")),
                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
                    name="Unassigned genes") %>%
        add_markers(data = enM_families, x = ~logFC, y = ~-log10(pvalue),
                    marker = list(symbol = c('square'), opacity = 0.8, line = list(width=0.6, color = "black"), size = ~increase_size*frequency),
                    color = ~factor(family), 
                    text = ~paste(gene, family, frequency, sep = "  "), hoverinfo = "text")
    } else{
      p <- plot_ly(colors = rainbow(howmany), width = 1200, height = 800) %>%
        #background
        add_markers(data = bpf, x = ~logFC, y = ~-log10(pvalue), opacity = 0.6,
                    marker = list(color = 'rgba(176,196,222,08)'),
                    text = ~paste(gene), hoverinfo = "text", showlegend = FALSE) %>%
        add_markers(data = enM_gna, x = ~logFC, y = ~-log10(pvalue), 
                    marker = list(size = 6, symbol = 2, color = c('white'), opacity = 0.8, line = list(width=0.9, color = "black")),
                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
                    name="Unassigned genes") %>%
        add_markers(data = enM_families, x = ~logFC, y = ~-log10(pvalue),
                    marker = list(symbol = c('square'), opacity = 0.8, line = list(width=0.6, color = "black"), size = 12),
                    color = ~factor(family), 
                    text = ~paste(gene, family, frequency, sep = "  "), hoverinfo = "text")
    }
    p
  })
  
  a_bpf_plus <- reactive({
    validate(
      need(!is.null(a_search_gene()), "")
    )
    p <- a_bpf()
    goi <- a_search_gene()
    orig_data <- a_pulldown()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  a_BPF_marker_sp <- reactive({
    inBPFmarker <- input$a_BPF_option_sp
    if (inBPFmarker == "change_m_BPF") {
      marker <- "change"
    } else {
      marker <- "no_change"
    }
  })
  
  a_bpf_data_sp <- eventReactive(input$a_make_bpf_sp, {
    validate(
      need(input$a_file_pulldown_r != '', "Upload file")
    )
    withProgress(message = 'This may take a while', 
                 detail = 'Hold please', value = 0, {
                   bpf <- a_pulldown()
                   
                   makePlotFamilies_1quadrant <- function(data, bait){
                     im <- subset(data, 
                                  (rep1 < input$a_BPF_rep1_range[2] & rep1 > input$a_BPF_rep1_range[1]) & 
                                    (rep2 < input$a_BPF_rep2_range[2] & rep2 > input$a_BPF_rep2_range[1]))
                     incProgress(0.6)
                     enM_families <- assignFamily_inc_doubles(im)
                     enM_gna <- addNames(im)
                     incProgress(0.8)
                     ### replace logFC and pvalue in di* by these from dpm
                     enM_families$logFC <- data$logFC[match(enM_families$gene, data$gene)]
                     enM_families$pvalue <- data$pvalue[match(enM_families$gene, data$gene)]
                     enM_gna$logFC <- data$logFC[match(enM_gna$gene, data$gene)]
                     enM_gna$pvalue <- data$pvalue[match(enM_gna$gene, data$gene)]
                     
                     # mybait <- data[grepl(bait,data$gene),]
                     
                     howmany <- length(unique(enM_families$family))
                     bpfmsizing <- a_BPF_marker_sp()
                     increase_size <- input$a_BPF_marker_freq_sp
                     
                     incProgress(0.9)
                     bpf_list <- list("list" = bpf, "list" = enM_families, "list" = enM_gna, "integer" = howmany, bpfmsizing, increase_size)
                     return(bpf_list)
                   }
                   makePlotFamilies_1quadrant(bpf)
                 })
  }) 
  
  a_bpf_sp <- reactive({
    bpf_data <- a_bpf_data_sp()
    bpf <- bpf_data[[1]]
    enM_families <- bpf_data[[2]]
    enM_gna <- bpf_data[[3]]
    howmany <- bpf_data[[4]]
    bpfmsizing <- bpf_data[[5]]
    increase_size <- bpf_data[[6]]
    
    if (bpfmsizing == "change"){
      p <- plot_ly(colors = rainbow(howmany)) #, width = 850, height = 800
      p <- add_lines(p, data = bpf, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                     text = "x=y", hoverinfo = "text",
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE) %>%
        #background
        add_markers(data = bpf, x = ~rep1, y = ~rep2, opacity = 0.6,
                    marker = list(color = 'rgba(176,196,222,08)'),
                    text = ~paste(gene), hoverinfo = "text", showlegend = FALSE) %>%
        add_markers(data = enM_gna, x = ~rep1, y = ~rep2, 
                    marker = list(size = 6, symbol = 2, color = c('white'), opacity = 0.8, line = list(width=0.9, color = "black")),
                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
                    name="Unassigned genes") %>%
        add_markers(data = enM_families, x = ~rep1, y = ~rep2,
                    marker = list(symbol = c('square'), opacity = 0.8, line = list(width=0.6, color = "black"), size = ~increase_size*frequency),
                    color = ~factor(family), 
                    text = ~paste(gene, family, frequency, sep = "  "), hoverinfo = "text")
    } else{
      p <- plot_ly(colors = rainbow(howmany)) #, width = 850, height = 800
      p <- add_lines(p, data = bpf, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                     text = "x=y", hoverinfo = "text",
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE) %>%
        #background
        add_markers(data = bpf, x = ~rep1, y = ~rep2, opacity = 0.6,
                    marker = list(color = 'rgba(176,196,222,08)'),
                    text = ~paste(gene), hoverinfo = "text", showlegend = FALSE) %>%
        add_markers(data = enM_gna, x = ~rep1, y = ~rep2, 
                    marker = list(size = 6, symbol = 2, color = c('white'), opacity = 0.8, line = list(width=0.9, color = "black")),
                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
                    name="Unassigned genes") %>%
        add_markers(data = enM_families, x = ~rep1, y = ~rep2,
                    marker = list(symbol = c('square'), opacity = 0.8, line = list(width=0.6, color = "black"), size = 12),
                    color = ~factor(family), 
                    text = ~paste(gene, family, frequency, sep = "  "), hoverinfo = "text")
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=c((min(bpf$rep1, bpf$rep2))-1, (max(bpf$rep1, bpf$rep2))+1)), 
             yaxis = list(title = "rep2", range=c((min(bpf$rep1, bpf$rep2))-1, (max(bpf$rep1, bpf$rep2))+1)))
  })
  
  a_bpf_plus_sp <- reactive({
    validate(
      need(!is.null(a_search_gene()), "")
    )
    p <- a_bpf_sp()
    goi <- a_search_gene()
    orig_data <- a_pulldown()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_scatter(p, searchgene)
    p1
  })
  
  a_bpf_sp_preview <- reactive({
    d <- a_pulldown()
    p <- plot_ly(showlegend = FALSE) 
    p <- add_lines(p, data = d, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                   text = "x=y", hoverinfo = "text",
                   line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
    p <- add_markers(p, data = d, x = ~rep1, y = ~rep2, 
                     marker = list(color = 'rgba(176,196,222,08)', size = 7, cmin = 0, cmax = 1, line = list(width=0.2, color = "grey89")), 
                     opacity = 0.9, 
                     text = ~paste0(gene, ", rep1=", rep1, ", rep2=", rep2), hoverinfo = "text", name = "pull down")
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1))) %>%
      add_lines(x = input$a_BPF_rep1_range[2], y = c(input$a_BPF_rep2_range[2], input$a_BPF_rep2_range[1]), line = list(width = 0.7, color = "#e41a1c"), 
                name = '', showlegend = F) %>%
      add_lines(x = (input$a_BPF_rep1_range[1]), y = c(input$a_BPF_rep2_range[2], input$a_BPF_rep2_range[1]), line = list(width = 0.7, color = "#e41a1c"),
                name = '', showlegend = F) %>%
      add_lines(x = c(input$a_BPF_rep1_range[2], input$a_BPF_rep1_range[1]), y = input$a_BPF_rep2_range[2], line = list(width = 0.7, color = "#e41a1c"),
                name = '', showlegend = F) %>%
      add_lines(x = c(input$a_BPF_rep1_range[2], input$a_BPF_rep1_range[1]), y = input$a_BPF_rep2_range[1], line = list(width = 0.7, color = "#e41a1c"),
                name = '', showlegend = F)
  })
  
  output$FDR_colorbar <- renderPlot({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    a_vp_colorbar()
  })
  
  output$VolcanoPlot <- renderPlotly({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file")
    )
    if(is.null(a_pf_db())){
      if(is.null(a_search_gene())){
        a_vp()
      } else{
        a_vp_plus_rep()
      }
    } else if(!is.null(a_pf_db())){
      a_vp_pf_db()
    }
  })
  
  output$VP_count <- renderTable({
    validate(
      need(input$a_file_pulldown_r != '', " ")
    )
    a_vp_count()
  }, rownames = T, bordered = T)
  
  output$ScatterPlot <- renderPlotly({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file")
    )
    input_file <- a_in_pulldown()
    d_col <- colnames(input_file)
    if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
       "rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(a_pf_db())){
        if(is.null(a_search_gene())){
          a_sp()
        } else{
          a_sp_plus()
        }
      } else if(!is.null(a_pf_db())){
        a_sp_pf_db()
      }
    } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
      validate(
        need("rep1" %in% d_col & "rep2" %in% d_col, "Must have rep1 and rep2 values.")
      )
    } else if("rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(a_pf_db())){
        if(is.null(a_search_gene())){
          a_sp()
        } else{
          a_sp_plus()
        }
      } else if(!is.null(a_pf_db())){
        a_sp_pf_db()
      }
    }
  })
  
  output$multi_FDR_colorbar <- renderPlot({
    validate(
      need(input$a_file_pulldown_r != '', "")
    )
    a_multi_vp_colorbar()
  })
  
  output$Multi_VolcanoPlot <- renderPlotly({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file")
    )
    if(is.null(a_search_gene())){
      a_multi_vp_layer()
    } else{
      a_multi_vp_plus()
    }
  })
  
  output$Multi_VP_count <- renderTable({
    validate(
      need(input$a_file_pulldown_r != '', " ")
    )
    a_multi_vp_count()
  }, rownames = T, bordered = T)
  
  output$Multi_VP_count_text <- renderUI({
    validate(
      need(input$a_file_pulldown_r != '', " ")
    )
    output <- a_multi_vp_count_text()
    HTML(paste(output$a, output$b, sep = "<br/>"))
  })
  
  output$Multi_ScatterPlot <- renderPlotly({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file")
    )
    input_file <- a_in_pulldown()
    d_col <- colnames(input_file)
    if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
       "rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(a_search_gene())){
        a_multi_sp_layer()
      } else{
        a_multi_sp_plus()
      }
    } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
      validate(
        need("rep1" %in% d_col & "rep2" %in% d_col, "Must have rep1 and rep2 values.")
      )
    } else if("rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(a_search_gene())){
        a_multi_sp_layer()
      } else{
        a_multi_sp_plus()
      }
    }
  })
  
  output$Venn_Diagram_bait <- renderPlot({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file"),
      need(input$a_bait_vennd != '', "Requires bait")
    )
    v0 <- a_venndiagram()
    grid.newpage()
    grid.draw(v0)
  })
  
  output$Venn_Diagram_GOI <- renderPlot({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file"),
      need(!is.null(a_upload_genes_vennd()), "Requires GOI")
    )
    v0 <- a_venndiagram_GOI()
    grid.newpage()
    grid.draw(v0)

  })
  
  output$Venn_Diagram_SNP <- renderPlot({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file"),
      need(!is.null(a_snp_vennd()), "Requires SNPs")
    )
    v0 <- a_venndiagram_SNP()
    grid.newpage()
    grid.draw(v0)
  })
  
  output$Venn_Diagram_SNP_SGL <- renderPlot({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file"),
      need(!is.null(a_snp_vennd()), "")
    )
    v0 <- a_venndiagram_SNP_SGL()
    grid.newpage()
    grid.draw(v0)
  })
  
  output$Venn_Diagram_SNP_MGL <- renderPlot({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file"),
      need(!is.null(a_snp_vennd()), "")
    )
    v0 <- a_venndiagram_SNP_MGL()
    grid.newpage()
    grid.draw(v0)
  })
  
  output$Basic_Protein_Family <- renderPlotly({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file")
    )
    if(is.null(a_search_gene())){
      a_bpf()
    } else{
      a_bpf_plus()
    }
  })
  
  output$Basic_Protein_Family_sp_prev <- renderPlotly({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file")
    )
    d <- a_orig_pulldown()
    d_col <- colnames(d)
    validate(
      need("rep1" %in% d_col & "rep2" %in% d_col, "")
    )
    a_bpf_sp_preview()
  })
  
  output$Basic_Protein_Family_sp <- renderPlotly({
    validate(
      need(input$a_file_pulldown_r != '', "Upload file")
    )
    if(is.null(a_search_gene())){
      a_bpf_sp()
    } else{
      a_bpf_plus_sp()
    }
  })

  observe({
    if(!is.null(input$a_file_pulldown_r)){
      df <- a_in_pulldown()
      if("rep1" %in% colnames(df) & "rep2" %in% colnames(df)){
        shinyjs::enable("download_replications_calculated")
      }
    }
  })
  
  observe({
    if(!is.null(input$a_file_pulldown_r)){
      df <- a_in_pulldown()
      if("accession_number" %in% colnames(df)){
        shinyjs::enable("download_mapped_uniprot")
      }
    }
  })
  
  observe({
    if (is.null(input$a_file_pulldown_r)){
      shinyjs::disable("download_basic_plots")
      shinyjs::disable("download_layered_plots")
      shinyjs::disable("download_venn_diagram_plot")
      shinyjs::disable("download_replications_calculated")
      shinyjs::disable("download_bpf_plot")
      shinyjs::disable("download_enriched_families_bpf")
      shinyjs::disable("download_venn_diagram_hypergeometric")
      shinyjs::disable("download_snp_to_genes")
      shinyjs::disable("download_venn_diagram_GOI_plot")
      shinyjs::disable("download_venn_diagram_SNP_plot")
      shinyjs::disable("download_mapped_uniprot")
    } else {
      shinyjs::enable("download_basic_plots")
    }
  })
  
  observe({
    if(!is.null(input$a_file_pulldown_r)){
      df <- a_in_pulldown()
      if("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df)){
        shinyjs::disable("download_replications_calculated")
      }
    }
  })
  
  observe({
    if(!is.null(input$a_file_pulldown_r)){
      df <- a_in_pulldown()
      if("gene" %in% colnames(df)){
        shinyjs::disable("download_mapped_uniprot")
      }
    }
  })
  
  observe({
    if (input$a_make_bpf == 0 || is.null(input$a_make_bpf)){
      shinyjs::disable("download_bpf_plot")
      shinyjs::disable("download_enriched_families_bpf")
    } else {
      shinyjs::enable("download_bpf_plot")
      shinyjs::enable("download_enriched_families_bpf")
    }
  })
  
  observe({
    if (input$a_make_vennd_bait == 0 || is.null(input$a_make_vennd_bait)){
      shinyjs::disable("download_venn_diagram_plot")
      shinyjs::hide("download_venn_diagram_inweb_genes")
    } else {
      shinyjs::enable("download_venn_diagram_plot")
      shinyjs::show("download_venn_diagram_inweb_genes")
    }
  })
  
  observe({
    if (input$a_make_vennd_snp == 0 || is.null(input$a_make_vennd_snp)){
      shinyjs::disable("download_venn_diagram_SNP_plot")
      shinyjs::hide("download_venn_diagram_SNP_genes")
    } else {
      shinyjs::enable("download_venn_diagram_SNP_plot")
      shinyjs::show("download_venn_diagram_SNP_genes")
    }
  })
  
  observe({
    if (input$a_make_vennd_goi == 0 || is.null(input$a_make_vennd_goi)){
      shinyjs::disable("download_venn_diagram_GOI_plot")
      shinyjs::hide("download_venn_diagram_GOI_genes")
    } else {
      shinyjs::enable("download_venn_diagram_GOI_plot")
      shinyjs::show("download_venn_diagram_GOI_genes")
    }
  })
  
  observe({
    if (is.null(input$a_file_SNP_rep)){
      shinyjs::disable("download_snp_to_genes")
    } else {
      shinyjs::enable("download_snp_to_genes")
    }
  })
  
  observe({
    if (input$a_make_plot == 0 || is.null(input$a_make_plot)){
      shinyjs::disable("download_layered_plots") 
    } else {
      shinyjs::enable("download_layered_plots") 
    }
  })

  observe({
    if(!is.null(a_pf_db())){
      if(!is.null(a_pulldown())){
        d <- a_orig_pulldown() 
        d_col <- colnames(d)
        if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
           "rep1" %in% d_col & "rep2" %in% d_col){
          shinyjs::hide("download_pf_cleaned_input")
        } else if("rep1" %in% d_col & "rep2" %in% d_col){
          shinyjs::show("download_pf_cleaned_input")
        } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col) {
          shinyjs::hide("download_pf_cleaned_input")
        }
      }
    } else if(is.null(a_pf_db())){
      shinyjs::hide("download_pf_cleaned_input")
    }
  })

  
  
  output$download_pf_cleaned_input <- downloadHandler(
    filename = function() {
      paste("input-pf-removed", ".txt", sep = "")
    },
    content = function(file) {
      write.table(a_pf_cleanup(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$download_mapped_uniprot <- downloadHandler(
    filename = function() {
      paste("protein-to-gene-names", ".txt", sep = "")
    },
    content = function(file) {
      write.table(a_converted(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$download_replications_calculated <- downloadHandler(
    filename = function() {
      paste("input_calculated", ".txt", sep = "")
    },
    content = function(file) {
      write.table(a_pulldown(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$download_basic_plots <- downloadHandler(
    filename = "quality-control.html",
    content = function(file) {
      df <- a_in_pulldown()
      if("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df)){
        if(is.null(a_search_gene())){
          params <- list(a = a_vp(), c = a_vp_colorbar_dl(), d = a_vp_count())
        } else{
          params <- list(a = a_vp_plus_rep(), c = a_vp_colorbar_dl(), d = a_vp_count())
        }
      } else if("rep1" %in% colnames(df) & "rep2" %in% colnames(df)){
        if(is.null(a_search_gene())){
          params <- list(a = a_vp(), b = a_sp(), c = a_vp_colorbar_dl(), d = a_vp_count())
        } else{
          params <- list(a = a_vp_plus_rep(), b = a_sp_plus(), c = a_vp_colorbar_dl(), d = a_vp_count())
        }
      }
      rmarkdown::render("scripts/basic.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$download_layered_plots <- downloadHandler(
    filename = "integrated-plots.html",
    content = function(file) {
      df <- a_in_pulldown()
      if("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df)){
        if(is.null(a_search_gene())){
          params <- list(a = a_multi_vp_layer(), c = a_multi_vp_colorbar_dl(), d = a_multi_vp_count())
        } else{
          params <- list(a = a_multi_vp_plus(), c = a_multi_vp_colorbar_dl(), d = a_multi_vp_count())
        }
      } else if("rep1" %in% colnames(df) & "rep2" %in% colnames(df)){
        if(is.null(a_search_gene())){
          params <- list(a = a_multi_vp_layer(), b = a_multi_sp_layer(), c = a_multi_vp_colorbar_dl(), d = a_multi_vp_count())
        } else{
          params <- list(a = a_multi_vp_plus(), b = a_multi_sp_plus(), c = a_multi_vp_colorbar_dl(), d = a_multi_vp_count())
        }
      }
      rmarkdown::render("scripts/basic.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$download_venn_diagram_plot <- downloadHandler(
    filename = "venn-diagram-InWeb.html",
    content = function(file) {
      params <- list(a = a_venndiagram(), b = hypergeometric_test_list(), c = input$a_FDR_range[1], d = input$a_FDR_range[2], 
                     e = a_bait_gene_vennd(), f = population_bait(), g = success_population_bait(), h = sample_bait(),
                     i = input$a_logFC_range[1], j = input$a_logFC_range[2])
      rmarkdown::render("scripts/vennd-inweb.Rmd", 
                        output_file = file,
                        params = params
                        # envir = new.env(parent = globalenv())
      )
    }
  )

  output$download_venn_diagram_inweb_genes <- downloadHandler(
    filename = function() {
      paste("inweb-overlap-genes", ".txt", sep = "")
    },
    content = function(file) {
      write.table(hypergeometric_test_list(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$download_venn_diagram_GOI_plot <- downloadHandler(
    filename = "venn-diagram-GOI.html",
    content = function(file) {
      params <- list(a = a_venndiagram_GOI(), b = hypergeometric_test_list_GOI(), c = input$a_FDR_range[1], d = input$a_FDR_range[2], 
                     f = population_GOI(), g = success_population_GOI(), h = sample_GOI(),
                     i = input$a_logFC_range[1], j = input$a_logFC_range[2])
      rmarkdown::render("scripts/vennd-GOI.Rmd", 
                        output_file = file,
                        params = params
                        # envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$download_venn_diagram_GOI_genes <- downloadHandler(
    filename = function() {
      paste("GOI-overlap-genes", ".txt", sep = "")
    },
    content = function(file) {
      write.table(hypergeometric_test_list_GOI(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$download_venn_diagram_SNP_plot <- downloadHandler(
    filename = "venn-diagram-SNP.html",
    content = function(file) {
      params <- list(a = a_venndiagram_SNP(), b = hypergeometric_test_list_SNP(), c = input$a_FDR_range[1], d = input$a_FDR_range[2], 
                     f = population_SNP(), g = success_population_SNP(), h = sample_SNP(),
                     i = input$a_logFC_range[1], j = input$a_logFC_range[2], k = a_venndiagram_SNP_SGL(), l = a_venndiagram_SNP_MGL(),
                     m = success_sample_SNP_SGL(), n = success_sample_SNP_MGL(), o = sample_SNP_SGL(), p = sample_SNP_MGL())
      rmarkdown::render("scripts/vennd-SNP.Rmd", 
                        output_file = file,
                        params = params
                        # envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$download_venn_diagram_SNP_genes <- downloadHandler(
    filename = function() {
      paste("SNP-overlap-genes", ".txt", sep = "")
    },
    content = function(file) {
      write.table(hypergeometric_test_list_SNP(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$download_bpf_plot <- downloadHandler(
    filename = "basic-protein-family.html",
    content = function(file) {
      if(is.null(a_search_gene())){
        params <- list(a = a_bpf())
      } else{
        params <- list(a = a_bpf_plus())
      }
      rmarkdown::render("scripts/pf.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$download_snp_to_genes <- downloadHandler(
    filename = function() {
      paste("snp-to-gene", ".txt", sep = "")
    },
    content = function(file) {
      write.table(SNP_to_gene(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$download_enriched_families_bpf <- downloadHandler(
    filename = function() {
      paste("enriched_protein_families", ".txt", sep = "")
    },
    content = function(file) {
      write.table(a_bpf_families(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  ##### VISUALIZATIONS END ##### 
  
  ##### ADVANCED FEATURES START ##### 
  
  #create slider for FDR
  output$b_PF_FDR_slider <- renderUI({
    validate(
      need(input$file_pfam1 != '', ""),
      need(input$file_pfam2 != '', ""),
      need(input$file_pfam3 != '', "")
    )
    sliderInput("b_PF_FDR_range", "Range for FDR",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for pvalue
  output$b_PF_pvalue_slider <- renderUI({
    validate(
      need(input$file_pfam1 != '', ""),
      need(input$file_pfam2 != '', ""),
      need(input$file_pfam3 != '', "")
    )
    sliderInput("b_PF_pvalue_range", "Range for pvalue",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  # based on a_pulldown(), create slider for BPF logFC
  output$b_PF_logFC_slider <- renderUI({
    validate(
      need(input$file_pfam1 != '', ""),
      need(input$file_pfam2 != '', ""),
      need(input$file_pfam3 != '', "")
    )
    if(!is.null(b_pulldown1()) & !is.null(b_pulldown2())){
      d1 <- b_pulldown1()
      d2 <- b_pulldown2()
      max_logFC <- max(max(d1$logFC), max(d2$logFC))
      max_logFC <- signif(max_logFC+0.5, 1)
      sliderInput("b_PF_logFC_range", "Range for logFC",
                  min = 0, max = max_logFC, value = c(0, max_logFC), step = 0.1)
    }
  })
  
  output$b_PF_marker_size <- renderUI({
    validate(
      need(input$file_pfam1 != '', ""),
      need(input$file_pfam2 != '', ""),
      need(input$file_pfam3 != '', "")
    )
    radioButtons('b_PF_option', 'Turn on/off marker sizing',
                 c(On = 'change_m_PF',
                   Off = 'static_m_PF'),
                 inline = T
    )
  })

  output$b_PF_freq <- renderUI({
    validate(
      need(input$file_pfam1 != '', ""),
      need(input$file_pfam2 != '', ""),
      need(input$file_pfam3 != '', "")
    )
    if(!is.null(input$b_PF_option)){
      inBPFmarker <- input$b_PF_option
      if (inBPFmarker == "change_m_PF") {
    sliderInput("b_PF_marker_freq", "Marker size",
                min = 1, max = 40, value = 1, step = 1)
      }
    }
  })
  
  output$b_PF_button <- renderUI({
    # if (input$file_pfam1 != 0 & input$file_pfam2 != 0 & input$file_pfam3 != 0){
    validate(
      need(input$file_pfam1 != '', ""),
      need(input$file_pfam2 != '', ""),
      need(input$file_pfam3 != '', "")
    )
    if(!is.null(b_pulldown1()) & !is.null(b_pulldown2()) & !is.null(b_pulldown3())){
      actionButton("b_make_pf", "Generate PF VP")
    }
  })
  
  output$b_GOI_search <- renderUI({
    textInput("b_goi_search", "Search for gene (e.g. SPOCK)")
  })
  
  b_search_gene <- reactive({
    gene_in <- input$b_goi_search
    if (is.null(gene_in) | gene_in == "" ){
      return(NULL)
    } else{
      gene <- gene_in
      gene <- toupper(gene)
    }
  })
  
  output$b_PF_sort_col <- renderUI({
    validate(
      need(input$file_pfam1 != '', ""),
      need(input$file_pfam2 != '', ""),
      need(input$file_pfam3 != '', "")
    )
    radioButtons('b_PF_sort_option', 'Sort',
                 c("Alphabetically" = 'sort_alph',
                   "PF Frequency" = 'sort_freq'),
                 inline = T
    )
  })
  
  b_PF_sorting <- reactive({
    inPFsort <- input$b_PF_sort_option
    if (inPFsort == "sort_alph") {
      marker <- "sort_a"
    } else {
      marker <- "sort_f"
    }
  })
  
  b_in_pulldown1 <- reactive({
    if(!is.null(input$file_pfam1)){
      pulldown <- input$file_pfam1
      d <- fread(pulldown$datapath, header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
      d <- na.omit(d)
    }
  })
  
  b_orig_pulldown1 <- reactive({
    if(!is.null(b_in_pulldown1())){
      d <- b_in_pulldown1()
      d_col <- colnames(d)
      if("gene" %in% d_col & "accession_number" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      } else if("accession_number" %in% d_col){
        withProgress(message = 'Mapping UniProt IDs to HGNC symbols',
                     detail = 'Hold please', value = 0, {
                       write.table(d$accession_number, "scripts/gene-tools-master/map/in.txt", append = F, quote = F,
                                   row.names = F, col.names = F)
                       incProgress(0.6)
                       system("python scripts/gene-tools-master/map/map.py")
                       incProgress(0.8)
                       d1 <- fread("scripts/gene-tools-master/map/results.txt", header = TRUE,
                                   sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
                       colnames(d1) <- c("uniprot_id", "gene", "method")
                       df <- merge(d, d1, by.x = "accession_number", by.y = "uniprot_id")
                       df <- subset(df, select=-c(method))
                       incProgress(0.9)
                       df
                     })
      } else if("gene" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      }
      df
    }
  })
  
  b_pulldown1 <- reactive({
    if(!is.null(b_orig_pulldown1())){
      d <- b_orig_pulldown1()
      d_col <- colnames(d)
      if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
         "rep1" %in% d_col & "rep2" %in% d_col){
        df1 <- d
      }else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
        df1 <- d
      }else if("rep1" %in% d_col & "rep2" %in% d_col){
        df <- d[,c("gene","rep1","rep2")]
        df1 <- calculate_moderated_ttest(df)
      }
      df1
    }
  })
  
  b_in_pulldown2 <- reactive({
    if(!is.null(input$file_pfam2)){
      pulldown <- input$file_pfam2
      d <- fread(pulldown$datapath, header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
      d <- na.omit(d)
    }
  })
  
  b_orig_pulldown2 <- reactive({
    if(!is.null(b_in_pulldown2())){
      d <- b_in_pulldown2()
      d_col <- colnames(d)
      if("gene" %in% d_col & "accession_number" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      } else if("accession_number" %in% d_col){
        withProgress(message = 'Mapping UniProt IDs to HGNC symbols',
                     detail = 'Hold please', value = 0, {
                       write.table(d$accession_number, "scripts/gene-tools-master/map/in.txt", append = F, quote = F,
                                   row.names = F, col.names = F)
                       incProgress(0.6)
                       system("python scripts/gene-tools-master/map/map.py")
                       incProgress(0.8)
                       d1 <- fread("scripts/gene-tools-master/map/results.txt", header = TRUE,
                                   sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
                       colnames(d1) <- c("uniprot_id", "gene", "method")
                       df <- merge(d, d1, by.x = "accession_number", by.y = "uniprot_id")
                       df <- subset(df, select=-c(method))
                       incProgress(0.9)
                       df
                     })
      } else if("gene" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      }
      df
    }
  })
  
  b_pulldown2 <- reactive({
    if(!is.null(b_orig_pulldown2())){
      d <- b_orig_pulldown2()
      d_col <- colnames(d)
      if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
         "rep1" %in% d_col & "rep2" %in% d_col){
        df1 <- d
      }else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
        df1 <- d
      }else if("rep1" %in% d_col & "rep2" %in% d_col){
        df <- d[,c("gene","rep1","rep2")]
        df1 <- calculate_moderated_ttest(df)
      }
      df1
    }
  })
  
  b_in_pulldown3 <- reactive({
    if(!is.null(input$file_pfam3)){
      pulldown <- input$file_pfam3
      d <- fread(pulldown$datapath, header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
      d <- na.omit(d)
    }
  })
  
  b_orig_pulldown3 <- reactive({
    if(!is.null(b_in_pulldown3())){
      d <- b_in_pulldown3()
      d_col <- colnames(d)
      if("gene" %in% d_col & "accession_number" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      } else if("accession_number" %in% d_col){
        withProgress(message = 'Mapping UniProt IDs to HGNC symbols',
                     detail = 'Hold please', value = 0, {
                       write.table(d$accession_number, "scripts/gene-tools-master/map/in.txt", append = F, quote = F,
                                   row.names = F, col.names = F)
                       incProgress(0.6)
                       system("python scripts/gene-tools-master/map/map.py")
                       incProgress(0.8)
                       d1 <- fread("scripts/gene-tools-master/map/results.txt", header = TRUE,
                                   sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
                       colnames(d1) <- c("uniprot_id", "gene", "method")
                       df <- merge(d, d1, by.x = "accession_number", by.y = "uniprot_id")
                       df <- subset(df, select=-c(method))
                       incProgress(0.9)
                       df
                     })
      } else if("gene" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      }
      df
    }
  })
  
  b_pulldown3 <- reactive({
    if(!is.null(b_orig_pulldown3())){
      d <- b_orig_pulldown3()
      d_col <- colnames(d)
      if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
         "rep1" %in% d_col & "rep2" %in% d_col){
        df1 <- d
      }else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
        df1 <- d
      }else if("rep1" %in% d_col & "rep2" %in% d_col){
        df <- d[,c("gene","rep1","rep2")]
        df1 <- calculate_moderated_ttest(df)
      }
      df1
    }
  })
  
  b_bait_gene <- reactive({
    bait_in <- input$b_bait_PF
    if (is.null(bait_in) | bait_in == "" ){
      return(NULL)
    } else{
      bait <- bait_in
      bait <- toupper(bait)
    }
  })
  
  b_PF_marker <- reactive({
    inBPFmarker <- input$b_PF_option
    if (inBPFmarker == "change_m_PF") {
      marker <- "change"
    } else {
      marker <- "no_change"
    }
  })
  
  b_pf_data <- eventReactive(input$b_make_pf, {
    withProgress(message = 'This may take a while', 
                 detail = 'Hold please', value = 0, {
                   pf1 <- b_pulldown1()
                   pf2 <- b_pulldown2()
                   pf3 <- b_pulldown3()
                   pfmsizing <- b_PF_marker()
                   incProgress(0.2)
                   
                   makePlotFamilies <- function(dim, dip, dpm, increase_size = input$b_PF_marker_freq){ #, bait
                     im <- subset(dim, 
                                  (pvalue < input$b_PF_pvalue_range[2] & pvalue > input$b_PF_pvalue_range[1]) & 
                                    (FDR < input$b_PF_FDR_range[2] & FDR > input$b_PF_FDR_range[1]) &
                                    (logFC < input$b_PF_logFC_range[2] & logFC > input$b_PF_logFC_range[1]))
                     ip <- subset(dip, 
                                  (pvalue < input$b_PF_pvalue_range[2] & pvalue > input$b_PF_pvalue_range[1]) & 
                                    (FDR < input$b_PF_FDR_range[2] & FDR > input$b_PF_FDR_range[1]) &
                                    (logFC < input$b_PF_logFC_range[2] & logFC > input$b_PF_logFC_range[1]))
                     pm <- dpm
                     
                     enM <- subset(im, gene %!in% ip$gene)
                     enP <- subset(ip, gene %!in% im$gene)
                     
                     incProgress(0.4)
                     enM_families <- assignFamily_inc_doubles(enM)
                     enM_gna <- addNames(enM)
                     enP_families <- assignFamily_inc_doubles(enP)
                     enP_gna <- addNames(enP)
                     
                     incProgress(0.6)
                     ### replace logFC and pvalue in di* by these from dpm
                     enM_families$logFC <- dpm$logFC[match(enM_families$gene, dpm$gene)]
                     enM_families$pvalue <- dpm$pvalue[match(enM_families$gene, dpm$gene)]
                     enP_families$logFC <- dpm$logFC[match(enP_families$gene, dpm$gene)]
                     enP_families$pvalue <- dpm$pvalue[match(enP_families$gene, dpm$gene)]
                     enM_gna$logFC <- dpm$logFC[match(enM_gna$gene, dpm$gene)]
                     enM_gna$pvalue <- dpm$pvalue[match(enM_gna$gene, dpm$gene)]
                     enP_gna$logFC <- dpm$logFC[match(enP_gna$gene, dpm$gene)]
                     enP_gna$pvalue <- dpm$pvalue[match(enP_gna$gene, dpm$gene)]
                     
                     # mybait <- dpm[grepl(bait,dpm$gene),]
                     
                     incProgress(0.8)
                     howmany1 <- length(unique(enM_families$family))
                     howmany2 <- length(unique(enP_families$family))
                     
                     # enM_families$family <- paste(enM_families$family, " ", sep = '')
                     
                     increase_size <- input$b_PF_marker_freq
                     
                     incProgress(0.9)
                     pf_list <- list("list" = dpm, "list" = enM_families, "list" = enM_gna, "list" = enP_families, "list" = enP_gna,
                                     "integer" = howmany1, "integer" = howmany2, pfmsizing, increase_size)
                     return(pf_list)
                   }
                   makePlotFamilies(pf1, pf2, pf3)
                 })
  })
  
  b_pf_families <- reactive({
    pf_data <- b_pf_data()
    enM_families <- pf_data[[2]]
    enP_families <- pf_data[[4]]
    families1 <- cbind(enM_families$gene, enM_families$family, enM_families$frequency)
    colnames(families1) <- c("gene", "family", "frequency")
    families2 <- cbind(enP_families$gene, enP_families$family, enP_families$frequency)
    colnames(families2) <- c("gene", "family", "frequency")
    families <- rbind(families1, families2)
    families
  })
  
  b_pf <- reactive({
    pf_data <- b_pf_data()
    pfsort <- b_PF_sorting()
    dpm <- pf_data[[1]]
    enM_families <- pf_data[[2]]
    enM_gna <- pf_data[[3]]
    enP_families <- pf_data[[4]]
    enP_gna <- pf_data[[5]]
    howmany1 <- pf_data[[6]]
    howmany2 <- pf_data[[7]]
    pfmsizing <- pf_data[[8]]
    increase_size <- pf_data[[9]]

    if(pfsort == "sort_f"){
      enM_families <- enM_families[order(-enM_families$frequency), ]
      if(nrow(enM_families)>=1){
        enM_families <- cbind(enM_families, new_f = paste0("(", enM_families$frequency, ") ", enM_families$family))
      } else {
        enM_families <- enM_families("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0), 
                               "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0), 
                               "gene" = character(0), "family" = character(0), "frequency" = numeric(0),
                               "new_f" = character(0))
      }
      enP_families <- enP_families[order(-enP_families$frequency), ]
      if(nrow(enP_families)>=1){
        enP_families <- cbind(enP_families, new_f = paste0("(", enP_families$frequency, ") ", enP_families$family))
      } else {
        enP_families <- enP_families("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0), 
                                     "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0), 
                                     "gene" = character(0), "family" = character(0), "frequency" = numeric(0),
                                     "new_f" = character(0))
      }
    } else if(pfsort == "sort_a"){
      enM_families 
      if(nrow(enM_families)>=1){
        enM_families <- cbind(enM_families, new_f = paste0(enM_families$family, " (", enM_families$frequency, ")"))
      } else {
        enM_families <- enM_families("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0),
                               "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0),
                               "gene" = character(0), "family" = character(0), "frequency" = numeric(0),
                               "new_f" = character(0))
      }
      enP_families 
      if(nrow(enP_families)>=1){
        enP_families <- cbind(enP_families, new_f = paste0(enP_families$family, " (", enP_families$frequency, ")"))
      } else {
        enP_families <- enP_families("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0),
                                     "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0),
                                     "gene" = character(0), "family" = character(0), "frequency" = numeric(0),
                                     "new_f" = character(0))
      }
    }
    
    enM_families$new_f <- paste(enM_families$new_f, " ", sep = '')
    enM_families$new_f <- factor(enM_families$new_f, levels = unique(enM_families$new_f))
    enP_families$new_f <- factor(enP_families$new_f, levels = unique(enP_families$new_f))

    if (pfmsizing == "change"){
      p <- plot_ly(colors = rainbow(howmany1+howmany2, start = 0, end = 0.85), width = 1300, height = 900) %>% # ,visible='legendonly'
        #background
        add_markers(data = dpm, x = ~logFC, y = ~-log10(pvalue), opacity = 0.6,
                    marker = list(color = 'rgba(176,196,222,08)'),
                    text = ~paste(gene), hoverinfo = "text", showlegend = FALSE) %>%
        # ### not assigned genes1
        add_markers(data = enM_gna, x = ~logFC, y = ~-log10(pvalue),
                    marker = list(size = 8, symbol = 14, color = c('white'), opacity = 0.8, line = list(width=0.9, color = "black")),
                    hoverinfo="text", text = ~paste(gene, name, sep = "  "), name = "Unassigned genes") %>%
        #families1
        add_markers(data = enM_families, x = ~logFC, y = ~-log10(pvalue),
                    marker = list(size = ~frequency*increase_size, symbol = c('square'), opacity = 0.8, line = list(width=0.6, color = "black")),
                    color = ~new_f,
                    text = ~paste(gene, family, frequency, sep = "  "), hoverinfo="text") %>%
        ### not assigned genes2
        add_markers(data = enP_gna, x = ~logFC, y = ~-log10(pvalue),
                    marker = list(size = 6, symbol = 2, color = c('white'), opacity = 0.8, line = list(width = 0.9, color = "black")),
                    hoverinfo="text", text = ~paste(gene, name, sep = "  "), name="Unassigned genes") %>%
        #families2
        add_markers(data = enP_families, x = ~logFC, y = ~-log10(pvalue), 
                    marker = list(size = ~frequency*increase_size, symbol = c('circle'), opacity = 0.8, line = list(width = 0.6, color = "black")), 
                    color = ~new_f,
                    text = ~paste(gene, family, frequency, sep = "  "), hoverinfo="text")
    } else{
      p <- plot_ly(colors = rainbow(howmany1+howmany2, start = 0, end = 0.85), width = 1300, height = 900) %>%
        #background
        add_markers(data = dpm, x = ~logFC, y = ~-log10(pvalue), opacity = 0.6,
                    marker = list(color = 'rgba(176,196,222,08)'),
                    text = ~paste(gene), hoverinfo = "text", showlegend = FALSE) %>%
        ### not assigned genes1
        add_markers(data = enM_gna, x = ~logFC, y = ~-log10(pvalue), 
                    marker = list(size = 8, symbol = 14, color = c('white'), opacity = 0.8, line = list(width=0.9, color = "black")), 
                    hoverinfo="text", text = ~paste(gene, name, sep = "  "), name = "Unassigned genes") %>%
        #families1
        add_markers(data = enM_families, x = ~logFC, y = ~-log10(pvalue), 
                    marker = list(symbol = c('square'), opacity = 1, line = list(width=0.6, color = "black"), size = 12), 
                    color = ~new_f,
                    text = ~paste(gene, family, frequency, sep = "  "), hoverinfo="text") %>%
        ### not assigned genes2
        add_markers(data = enP_gna, x = ~logFC, y = ~-log10(pvalue), 
                    marker = list(size = 6, symbol = 2, color = c('white'), opacity = 0.8, line = list(width = 0.9, color = "black")), 
                    hoverinfo="text", text = ~paste(gene, name, sep = "  "), name="Unassigned genes") %>%
        #families2
        add_markers(data = enP_families, x = ~logFC, y = ~-log10(pvalue), 
                    marker = list(symbol = c('circle'), opacity = 1, line = list(width = 0.6, color = "black"), size = 12), 
                    color = ~new_f,
                    text = ~paste(gene, family, frequency, sep = "  "), hoverinfo="text") 
    }
    p
  })
  
  b_pf_plus <- reactive({
    validate(
      need(!is.null(b_search_gene()), "")
    )
    p <- b_pf()
    goi <- b_search_gene()
    orig_data <- b_pulldown3()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    
    p %>%
      add_trace(data = searchgene, x = ~logFC, y = ~-log10(pvalue), 
                mode = "markers+text", hoverinfo="text+x+y", text = ~paste(gene), 
                marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
                textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10), 
                type = 'scatter', showlegend = FALSE)
  })
  
  output$Protein_Family <- renderPlotly({
    validate(
      need(input$file_pfam1 != '', ""),
      need(input$file_pfam2 != '', ""),
      need(input$file_pfam3 != '', "")
    )
    if(is.null(b_search_gene())){
      b_pf()
    } else{
      b_pf_plus()
    }
  })
  
  observe({
    # if (is.null(input$input$file_pfam1) & is.null(input$file_pfam2) & is.null(input$file_pfam3)){
    if (input$b_make_pf == 0 || is.null(input$b_make_pf)){
      shinyjs::disable("download_pf_plot")
      shinyjs::disable("download_enriched_families_pf")
    } else {
      shinyjs::enable("download_pf_plot")
      shinyjs::enable("download_enriched_families_pf")
    }
  })
  
  output$download_pf_plot <- downloadHandler(
    filename = "advanced-protein-family.html",
    content = function(file) {
      if(is.null(b_search_gene())){
        params <- list(a = b_pf())
      } else{
        params <- list(a = b_pf_plus())
      }
      rmarkdown::render("scripts/pf.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$download_enriched_families_pf <- downloadHandler(
    filename = function() {
      paste("advanced_enriched_families", ".txt", sep = "")
    },
    content = function(file) {
      write.table(b_pf_families(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  ##### ADVANCED FEATURES END ##### 
  
  ##### MULTIPLE PD COMPARISONS START #####

  output$c_file1 <- renderUI({
    fileInput('c_file_pulldown1', 'Upload user input file1',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv')
    )
  })
  
  output$c_file2 <- renderUI({
    fileInput('c_file_pulldown2', 'Upload user input file2',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv')
    )
  })

  output$c_file3 <- renderUI({
    fileInput('c_file_pulldown3', 'Upload user input file3',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv')
    )
  })
  
  output$c_file4 <- renderUI({
    fileInput('c_file_pulldown4', 'Upload user input file4',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv')
    )
  })
  
  output$c_color_scheme <- renderUI({
    radioButtons("c_colorscheme", "Color scheme:",
                 c("FDR" = "fdr", 
                   "ExAC" = "exac",
                   "Grayscale" = "cbf"),
                 inline = T)
  })
  
  output$c_GOI_search <- renderUI({
    textInput("c_goi_search", "Search for gene (e.g. SHH)")
  })
  
  output$c_FDR_thresh <- renderUI({
    sliderInput("c_fdr_thresh", "FDR threshold",
                min = 0, max = 1, value = 0.1, step = 0.01)
  })
  
  output$c_PVal_thresh <- renderUI({
    sliderInput("c_pval_thresh", "p-value threshold",
                min = 0, max = 1, value = 0.05, step = 0.001)
  })
  
  output$c_logFC_thresh_combined <- renderUI({
    if(!is.null(c_in_pd1()) & is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      min_logFC <- min(c1$logFC)
      min_logFC <- signif(min_logFC-0.5, 1)
      max_logFC <- max(c1$logFC)
      max_logFC <- signif(max_logFC+0.5, 1)
      combined_max <- max(-(min_logFC), max_logFC)
      numericInput("c_logfc_thresh_comb", "logFC threshold",
                   min = 0, max = combined_max, value = 1, step = 0.1)
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      min_logFC <- min(min(c1$logFC), min(c2$logFC))
      min_logFC <- signif(min_logFC-0.5, 1)
      max_logFC <- max(max(c1$logFC), max(c2$logFC))
      max_logFC <- signif(max_logFC+0.5, 1)
      combined_max <- max(-(min_logFC), max_logFC)
      numericInput("c_logfc_thresh_comb", "logFC threshold",
                   min = 0, max = combined_max, value = 1, step = 0.1)
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & !is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      min_logFC <- min(min(c1$logFC), min(c2$logFC), min(c3$logFC))
      min_logFC <- signif(min_logFC-0.5, 1)
      max_logFC <- max(max(c1$logFC), max(c2$logFC), max(c3$logFC))
      max_logFC <- signif(max_logFC+0.5, 1)
      combined_max <- max(-(min_logFC), max_logFC)
      numericInput("c_logfc_thresh_comb", "logFC threshold",
                   min = 0, max = combined_max, value = 1, step = 0.1)
    } else
      numericInput("c_logfc_thresh_comb", "logFC threshold",
                  min = 0, max = 1, value = 0.1, step = 0.1)
  })
  
  #create slider for FDR f1
  output$c_comparison1_FDR_slider <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    sliderInput("c_compare1_FDR_range", "FDR",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for FDR f2
  output$c_comparison2_FDR_slider <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    sliderInput("c_compare2_FDR_range", "FDR",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for FDR f3
  output$c_comparison3_FDR_slider <- renderUI({
    validate(
      need(input$c_file_pulldown3 != '', "")
    )
    sliderInput("c_compare3_FDR_range", "FDR",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for pvalue f1
  output$c_comparison1_pvalue_slider <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    sliderInput("c_compare1_pvalue_range", "pvalue",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for pvalue f2
  output$c_comparison2_pvalue_slider <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    sliderInput("c_compare2_pvalue_range", "pvalue",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for pvalue f3
  output$c_comparison3_pvalue_slider <- renderUI({
    validate(
      need(input$c_file_pulldown3 != '', "")
    )
    sliderInput("c_compare3_pvalue_range", "pvalue",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slide for logFC FILE1
  output$c_comparison_logFC_slider1 <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    df <- c_pd1()
    min_logFC <- min(df$logFC)
    min_logFC <- signif(min_logFC-0.5, 1)
    max_logFC <- max(df$logFC)
    max_logFC <- signif(max_logFC+0.5, 1)
    sliderInput("c_f1_logFC_range", "logFC",
                min = min_logFC, max = max_logFC, value = c(1, max_logFC), step = 0.1)
  })
  
  #create slide for logFC FILE2
  output$c_comparison_logFC_slider2 <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    df <- c_pd2()
    min_logFC <- min(df$logFC)
    min_logFC <- signif(min_logFC-0.5, 1)
    max_logFC <- max(df$logFC)
    max_logFC <- signif(max_logFC+0.5, 1)
    sliderInput("c_f2_logFC_range", "logFC",
                min = min_logFC, max = max_logFC, value = c(1, max_logFC), step = 0.1)
  })
  
  #create slide for logFC FILE3
  output$c_comparison_logFC_slider3 <- renderUI({
    validate(
      need(input$c_file_pulldown3 != '', "")
    )
    df <- c_pd3()
    min_logFC <- min(df$logFC)
    min_logFC <- signif(min_logFC-0.5, 1)
    max_logFC <- max(df$logFC)
    max_logFC <- signif(max_logFC+0.5, 1)
    sliderInput("c_f3_logFC_range", "logFC",
                min = min_logFC, max = max_logFC, value = c(1, max_logFC), step = 0.1)
  })
  
  output$c_bait_layer <- renderUI({
    textInput("c_bait", "Input HGNC symbol to search for InWeb protein interactors (e.g. ZBTB7A)")
  })
  
  output$c_text_inweb <- renderUI({
    radioButtons('c_marker_text_inweb', 'Turn on/off labels',
                 c(On = 'yes_label',
                   Off = 'no_label'),
                 inline = T
    )
  })
  
  output$c_genes_file <- renderUI({
    fileInput('c_file_genes', 'File containing at least 2 genes with header, one HGNC symbol per line (e.g. TINMAN)',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'), multiple = T
    )
  })
  
  output$c_SNP_file <- renderUI({
    fileInput('c_file_SNP', 'File containing list of SNPs, one ID per line (e.g. rs12493885)',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv')
    )
  })
  
  output$c_text_goi <- renderUI({
    radioButtons('c_marker_text_goi', 'Turn on/off labels',
                 c(On = 'yes_label',
                   Off = 'no_label'),
                 inline = T
    )
  })
  
  output$c_text_snp <- renderUI({
    radioButtons('c_marker_text_snp', 'Turn on/off labels',
                 c(On = 'yes_label',
                   Off = 'no_label'),
                 inline = T
    )
  })
  
  output$c_button_inweb <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', "")
    )
    if(!is.null(c_bait_in())){
      actionButton("c_make_plot_inweb", "Generate plots")
    }
  })
  
  output$c_button_goi <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', "")
    )
    if(!is.null(c_upload_genes())){
      actionButton("c_make_plot_goi", "Generate plots")
    }
  })
  
  output$c_button_snp <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', "")
    )
    if(!is.null(c_snp())){
      actionButton("c_make_plot_snp", "Generate plots")
    }
  })
  
  #create slider for FDR
  output$c_pf_FDR_slider1 <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    sliderInput("c_f1_pf_FDR", "FDR",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for FDR file2
  output$c_pf_FDR_slider2 <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    sliderInput("c_f2_pf_FDR", "FDR",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for FDR file3
  output$c_pf_FDR_slider3 <- renderUI({
    validate(
      need(input$c_file_pulldown3 != '', "")
    )
    sliderInput("c_f3_pf_FDR", "FDR",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for pvalue
  output$c_pf_pvalue_slider1 <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    sliderInput("c_f1_pf_pvalue", "pvalue",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for pvalue file2
  output$c_pf_pvalue_slider2 <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    sliderInput("c_f2_pf_pvalue", "pvalue",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slider for pvalue file3
  output$c_pf_pvalue_slider3 <- renderUI({
    validate(
      need(input$c_file_pulldown3 != '', "")
    )
    sliderInput("c_f3_pf_pvalue", "pvalue",
                min = 0, max = 1, value = c(0, 1), step = 0.05, sep = '', pre = NULL, post = NULL)
  })
  
  #create slide for logFC FILE1
  output$c_pf_logFC_slider1 <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    df <- c_pd1()
    min_logFC <- min(df$logFC)
    min_logFC <- signif(min_logFC-0.5, 1)
    max_logFC <- max(df$logFC)
    max_logFC <- signif(max_logFC+0.5, 1)
    sliderInput("c_f1_pf_logFC_range", "logFC",
                min = min_logFC, max = max_logFC, value = c(1, max_logFC), step = 0.1)
  })
  
  #create slide for logFC FILE2
  output$c_pf_logFC_slider2 <- renderUI({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    df <- c_pd2()
    min_logFC <- min(df$logFC)
    min_logFC <- signif(min_logFC-0.5, 1)
    max_logFC <- max(df$logFC)
    max_logFC <- signif(max_logFC+0.5, 1)
    sliderInput("c_f2_pf_logFC_range", "logFC",
                min = min_logFC, max = max_logFC, value = c(1, max_logFC), step = 0.1)
  })
  
  #create slide for logFC FILE3
  output$c_pf_logFC_slider3 <- renderUI({
    validate(
      need(input$c_file_pulldown3 != '', "")
    )
    df <- c_pd3()
    min_logFC <- min(df$logFC)
    min_logFC <- signif(min_logFC-0.5, 1)
    max_logFC <- max(df$logFC)
    max_logFC <- signif(max_logFC+0.5, 1)
    sliderInput("c_f3_pf_logFC_range", "logFC",
                min = min_logFC, max = max_logFC, value = c(1, max_logFC), step = 0.1)
  })
  
  output$c_PF_button <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', ""),
      need(input$c_file_pulldown2 != '', "")
    )
    if(!is.null(c_compare1()) & !is.null(c_compare2())){
      actionButton("c_make_bpf", "Generate PF plot")
    }
  })
  
  output$c_PF_marker_size <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', ""),
      need(input$c_file_pulldown2 != '', "")
    )
    radioButtons('c_PF_option', 'Turn on/off marker sizing',
                 c(On = 'change_m_PF',
                   Off = 'static_m_PF'),
                 inline = T
    )
  })
  
  c_PF_marker <- reactive({
    inPFmarker <- input$c_PF_option
    if (inPFmarker == "change_m_PF") {
      marker <- "change"
    } else {
      marker <- "no_change"
    }
  })
  
  output$c_PF_sort_col <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', ""),
      need(input$c_file_pulldown2 != '', "")
    )
    radioButtons('c_PF_sort_option', 'Sort',
                 c("Alphabetically" = 'sort_alph',
                   "PF Frequency" = 'sort_freq'),
                 inline = T
    )
  })
  
  c_PF_sorting <- reactive({
    inPFsort <- input$c_PF_sort_option
    if (inPFsort == "sort_alph") {
      marker <- "sort_a"
    } else {
      marker <- "sort_f"
    }
  })
  
  output$c_PF_freq <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', ""),
      need(input$c_file_pulldown2 != '', "")
    )
    if(!is.null(input$c_PF_option)){
      inPFmarker <- input$c_PF_option
      if (inPFmarker == "change_m_PF") {
        sliderInput("c_PF_marker_freq", "Marker size",
                    min = 1, max = 40, value = 1, step = 1)     
      }
    }
  })
  
  output$c_prot_fam_db <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', "")
    )
    selectInput('c_pfam_db', 'Protein families', colnames(prot_fam), multiple=TRUE, selectize=TRUE)
  })
  
  output$c_text_prot_fam_db <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', "")
    )
    radioButtons('c_marker_text_prot_fam_db', 'Turn on/off labels',
                 c(On = 'yes_label',
                   Off = 'no_label'),
                 inline = T
    )
  })
  
  observe({
    if(input$comparison == "3_p2" | input$comparison == "3_p6"){
      shinyjs::hide("c_colorscheme")
      shinyjs::hide("c_fdr_thresh")
      shinyjs::hide("c_pval_thresh")
      shinyjs::hide("c_logfc_thresh_comb")
    } else {
      shinyjs::show("c_colorscheme")
      shinyjs::show("c_fdr_thresh")
      shinyjs::show("c_pval_thresh")
      shinyjs::show("c_logfc_thresh_comb")
    }
  })
  
  c_search_gene <- reactive({
    gene_in <- input$c_goi_search
    if (is.null(gene_in) | gene_in == "" ){
      return(NULL)
    } else{
      gene <- gene_in
      gene <- toupper(gene)
    }
  })
  
  c_bait_in <- reactive({
    bait_in <- input$c_bait
    if (is.null(bait_in) | bait_in == "" ){
      return(NULL)
    } else{
      bait <- bait_in
      bait <- toupper(bait)
    }
  })
  
  c_bait_friends <- reactive({
    if(!is.null(c_bait_in())){
      withProgress(message = 'Finding bait interactors in InWeb', 
                   detail = 'Hold please', value = 0, {
                     bait <- c_bait_in()
                     bait_inweb3 <-
                       system(paste("grep -w", bait, "data/InWeb3_interactions.txt | sed -e 's/(//g' | tr '\t' '\\n' | sort -u | grep -v", bait, sep = " "), intern = TRUE)
                     incProgress(0.5)
                     bait_inweb_im <- 
                       system(paste("zgrep -w", bait, "data/core.psimitab.gz | awk -v FS=\"(uniprotkb:|gene name)\" '{print $6, $9}' | sed -e 's/(//g' | tr ' ' '\\n' | sort -u | grep -v", bait, sep = " "), intern = TRUE)
                     incProgress(0.8)
                   })
      bait_interactors <- c(bait_inweb_im, bait_inweb3)
      bait_interactors <- unique(sort(bait_interactors))
    }
  })
  
  c_upload_genes <- reactive({
    genes <- input$c_file_genes
    if (is.null(genes)){
      return(NULL)
    } else{
      df <- fread(genes$datapath, header = T, fill = T,
                  sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
    }
  })
  
  c_genes_uploaded <- reactive({
    if(!is.null(c_upload_genes())){
      genes <- c_upload_genes()
      genes <- as.data.frame(sapply(genes, toupper)) 
      genes
    }
  })
  
  output$c_goi_num_inputs <- renderUI({
    validate(
      need(!is.null(c_vp1_goi_layer()), ""),
      need(!is.null(c_vp2_goi_layer()), "")
    )
    d <- c_pd1()
    gene_interest <- c_genes_uploaded()
    d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )  
    choices <- names(d_g2s)
    choices <- append(choices, "total")
    list_count <- length(d_g2s)
    if(list_count>0){
      selectInput("c_goi_num_inputs", "GOI list",
                  choices = choices, multiple = F)
    }
  })
  
  output$snp_num_inputs <- renderUI({
    validate(
      need(!is.null(c_vp1_snp_layer()), ""),
      need(!is.null(c_vp2_snp_layer()), "")
    )
    snp2gene <- c_SNP_to_gene()
    snp_interest <- split(snp2gene, snp2gene$snpid)
    choices <- names(snp_interest)
    choices <- append(choices, "total")
    list_count <- length(snp_interest)
    if(list_count>0){
      selectInput("c_snp_num_inputs", "SNP input",
                  choices = choices, multiple = F)
    }
  })
  
  c_snp <- reactive({
    snp <- input$c_file_SNP
    if(is.null(snp)){
      return(NULL)
    } else{
      df <- read.csv(snp$datapath, header = FALSE)
      df
    }
  })
  
  #snp to gene using LD r^2>0.6±50kb
  c_SNP_to_gene <- reactive({
    if(!is.null(c_snp())){
      withProgress(message = 'Finding genes in SNPs loci', 
                   detail = "Hold please", value = 0, {
                     snp_data <- c_snp()
                     incProgress(0.2)
                     write.table(snp_data, file = "data/snp.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
                     incProgress(0.6)
                     system("scripts/genes_in_loci.sh")
                     incProgress(0.8)
                     SNPgeneList <- read.table("data/snp_to_gene.txt", header = TRUE)
                     n_occur <- data.frame(table(SNPgeneList$snpid))
                     incProgress(0.9)
                     SNP_n_occur <- merge(SNPgeneList, n_occur, by.x = 'snpid', by.y = 'Var1')
                   })
      snpList <- SNP_n_occur
    }
  })
  
  c_inweb_pd1 <- eventReactive(input$c_make_plot_inweb, {
    if(!is.null(c_bait_friends())){
      bait_interactors <- c_bait_friends()
      d <- c_pd1()
      d_subset <- subset(d, gene %in% bait_interactors)
      rownames(d_subset) <- NULL
      d_subset
    }
  })
  
  c_inweb_pd2 <- eventReactive(input$c_make_plot_inweb, {
    if(!is.null(c_bait_friends())){
      bait_interactors <- c_bait_friends()
      d <- c_pd2()
      d_subset <- subset(d, gene %in% bait_interactors)
      rownames(d_subset) <- NULL
      d_subset
    }
  })
  
  c_inweb_pd3 <- eventReactive(input$c_make_plot_inweb, { 
    if(!is.null(c_bait_friends())){
      bait_interactors <- c_bait_friends()
      d <- c_pd3()
      d_subset <- subset(d, gene %in% bait_interactors)
      rownames(d_subset) <- NULL
      d_subset
    }
  })

  c_in_pd1 <-reactive({
    if(!is.null(input$c_file_pulldown1)){
      pulldown <- input$c_file_pulldown1
      d <- fread(pulldown$datapath, header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
      d <- na.omit(d)
    }
  })
  
  c_orig_pd1 <- reactive({
    if(!is.null(c_in_pd1())){
      d <- c_in_pd1()
      d_col <- colnames(d)
      if("gene" %in% d_col & "accession_number" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      } else if("accession_number" %in% d_col){
        withProgress(message = 'Mapping UniProt IDs to HGNC symbols',
                     detail = 'Hold please', value = 0, {
                       write.table(d$accession_number, "scripts/gene-tools-master/map/in.txt", append = F, quote = F,
                                   row.names = F, col.names = F)
                       incProgress(0.6)
                       system("python scripts/gene-tools-master/map/map.py")
                       incProgress(0.8)
                       d1 <- fread("scripts/gene-tools-master/map/results.txt", header = TRUE,
                                   sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
                       colnames(d1) <- c("uniprot_id", "gene", "method")
                       df <- merge(d, d1, by.x = "accession_number", by.y = "uniprot_id")
                       df <- subset(df, select=-c(method))
                       incProgress(0.9)
                       df
                     })
      } else if("gene" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      }
      df
    }
  })
  
  c_pd1 <- reactive({
    if(!is.null(c_orig_pd1())){
      d <- c_orig_pd1()
      d_col <- colnames(d)
      if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
         "rep1" %in% d_col & "rep2" %in% d_col){
        df1 <- d
      }else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
        df1 <- d
      }else if("rep1" %in% d_col & "rep2" %in% d_col){
        df <- d[,c("gene","rep1","rep2")]
        df1 <- calculate_moderated_ttest(df)
      }
      df1
    }
  })
  
  c_pd1_converted <- reactive({
    if(!is.null(c_orig_pd1())){
      d <- fread("scripts/gene-tools-master/map/f1_res.txt", header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
    }
  })
  
  c_in_pd2 <-reactive({
    if(!is.null(input$c_file_pulldown2)){
      pulldown <- input$c_file_pulldown2
      d <- fread(pulldown$datapath, header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
      d <- na.omit(d)
    }
  })
  
  c_orig_pd2 <- reactive({
    if(!is.null(c_in_pd2())){
      d <- c_in_pd2()
      d_col <- colnames(d)
      if("gene" %in% d_col & "accession_number" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      } else if("accession_number" %in% d_col){
        withProgress(message = 'Mapping UniProt IDs to HGNC symbols',
                     detail = 'Hold please', value = 0, {
                       write.table(d$accession_number, "scripts/gene-tools-master/map/in.txt", append = F, quote = F,
                                   row.names = F, col.names = F)
                       incProgress(0.6)
                       system("python scripts/gene-tools-master/map/map.py")
                       incProgress(0.8)
                       d1 <- fread("scripts/gene-tools-master/map/results.txt", header = TRUE,
                                   sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
                       colnames(d1) <- c("uniprot_id", "gene", "method")
                       df <- merge(d, d1, by.x = "accession_number", by.y = "uniprot_id")
                       df <- subset(df, select=-c(method))
                       incProgress(0.9)
                       df
                     })
      } else if("gene" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      }
      df
    }
  })

  c_pd2 <- reactive({
    if(!is.null(c_orig_pd2())){
      d <- c_orig_pd2()
      d_col <- colnames(d)
      if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
         "rep1" %in% d_col & "rep2" %in% d_col){
        df1 <- d
      }else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
        df1 <- d
      }else if("rep1" %in% d_col & "rep2" %in% d_col){
        df <- d[,c("gene","rep1","rep2")]
        df1 <- calculate_moderated_ttest(df)
      }
      df1
    }
  })
  
  c_pd2_converted <- reactive({
    if(!is.null(c_orig_pd2())){
      d <- fread("scripts/gene-tools-master/map/f2_res.txt", header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
    }
  })
  
  c_in_pd3 <-reactive({
    if(!is.null(input$c_file_pulldown3)){
      pulldown <- input$c_file_pulldown3
      d <- fread(pulldown$datapath, header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE, blank.lines.skip = T)
      d <- na.omit(d)
    }
  })
  
  c_orig_pd3 <- reactive({
    if(!is.null(c_in_pd3())){
      d <- c_in_pd3()
      d_col <- colnames(d)
      if("gene" %in% d_col & "accession_number" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      } else if("accession_number" %in% d_col){
        withProgress(message = 'Mapping UniProt IDs to HGNC symbols',
                     detail = 'Hold please', value = 0, {
                       write.table(d$accession_number, "scripts/gene-tools-master/map/in.txt", append = F, quote = F,
                                   row.names = F, col.names = F)
                       incProgress(0.6)
                       system("python scripts/gene-tools-master/map/map.py")
                       incProgress(0.8)
                       d1 <- fread("scripts/gene-tools-master/map/results.txt", header = TRUE,
                                   sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
                       colnames(d1) <- c("uniprot_id", "gene", "method")
                       df <- merge(d, d1, by.x = "accession_number", by.y = "uniprot_id")
                       df <- subset(df, select=-c(method))
                       incProgress(0.9)
                       df
                     })
      } else if("gene" %in% d_col){
        df <- d
        df$gene <- toupper(df$gene)
      }
      df
    }
  })
  
  c_pd3 <- reactive({
    if(!is.null(c_orig_pd3())){
      d <- c_orig_pd3()
      d_col <- colnames(d)
      if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
         "rep1" %in% d_col & "rep2" %in% d_col){
        df1 <- d
      }else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
        df1 <- d
      }else if("rep1" %in% d_col & "rep2" %in% d_col){
        df <- d[,c("gene","rep1","rep2")]
        df1 <- calculate_moderated_ttest(df)
      }
      df1
    }
  })
  
  c_pd3_converted <- reactive({
    if(!is.null(c_pd3())){
      d <- fread("scripts/gene-tools-master/map/f3_res.txt", header = TRUE,
                 sep="auto", na.strings=c(""," ","NA"), stringsAsFactors = FALSE, data.table = FALSE)
    }
  })
  
  c_min_x <- reactive({
    if(!is.null(c_in_pd1()) & is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      min_x_axis <- min(c1$logFC)
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      min_x_axis <- min(min(c1$logFC), min(c2$logFC))
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & !is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      min_x_axis <- min(min(c1$logFC), min(c2$logFC), min(c3$logFC))
    }
    min_x_axis
  })
  
  c_max_x <- reactive({
    if(!is.null(c_in_pd1()) & is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      max_x_axis <- max(c1$logFC)
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      max_x_axis <- max(max(c1$logFC), max(c2$logFC))
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & !is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      max_x_axis <- max(max(c1$logFC), max(c2$logFC), max(c3$logFC))
    }
    max_x_axis
  })
  
  c_min_y <- reactive({
    if(!is.null(c_in_pd1()) & is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      min_y_axis <- min(-log10(c1$pvalue))
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      min_y_axis <- min(min(-log10(c1$pvalue)), min(-log10(c2$pvalue)))
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & !is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      min_y_axis <- min(min(-log10(c1$pvalue)), min(-log10(c2$pvalue)), min(-log10(c3$pvalue)))
    }
    min_y_axis
  })
  
  c_max_y <- reactive({
    if(!is.null(c_in_pd1()) & is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      max_y_axis <- max(-log10(c1$pvalue))
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      max_y_axis <- max(max(-log10(c1$pvalue)), max(-log10(c2$pvalue)))
    } else if(!is.null(c_in_pd1()) & !is.null(c_in_pd2()) & !is.null(c_in_pd3())){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      max_y_axis <- max(max(-log10(c1$pvalue)), max(-log10(c2$pvalue)), max(-log10(c3$pvalue)))
    }
    max_y_axis
  })
  
  c_vp_colorbar_dl <- reactive({
    FDR <- seq(0, 1, 0.01)
    limit <- rep("FDR", 101)
    d <- data.frame(limit, FDR)
    if(input$c_colorscheme == "fdr"){
      d1 <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = d1$FDR-0.01, xmax = d1$FDR, ymin = 0, ymax = 0.1)) + geom_rect(fill = mycol) +
        scale_x_continuous(breaks = seq(0, 1, 0.1)) +
        labs(x = "FDR") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$c_colorscheme == "exac"){
      d1 <- separate_to_groups_for_exac_bar(d)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = d1$FDR-0.01, xmax = d1$FDR, ymin = 0, ymax = 0.1)) + geom_rect(fill = mycol) +
        labs(x = " pLI < 0.9          pLI >= 0.9       not in ExAC") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    } else if(input$c_colorscheme == "cbf"){
      d1 <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      mycol <- as.vector(d1$col)
      bar <- ggplot(d1, aes(xmin = d1$FDR-0.01, xmax = d1$FDR, ymin = 0, ymax = 0.1)) + geom_rect(fill = mycol) +
        scale_x_continuous(breaks = seq(0, 1, 0.1)) +
        labs(x = "FDR") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              axis.title = element_text(size = rel(1))) + coord_fixed()
      bar
    }
  })
  
  c_vp1 <- reactive({
    d <- c_pd1()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    }
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    p <- p %>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp1_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp1()
    goi <- c_search_gene()
    orig_data <- c_pd1()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_vp1_inweb <- eventReactive(input$c_make_plot_inweb, {
    validate(
      need(!is.null(c_pd1()), "")
    )
    d <- c_pd1()
    if(!is.null(c_inweb_pd1())){
      inwebFile <- c_inweb_pd1()
      bait <- c_bait_in()
      d_in <- subset(d, gene %in% inwebFile$gene & gene != bait)
      list(d_in=d_in)
    }
  })
  
  c_vp1_inweb_layer <- reactive({
    validate(
      need(!is.null(c_pd1()), "")
    )
    d <- c_pd1()
    c1_multi_vp <- c_vp1_inweb()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    vp_title <- c_pd1_inweb_hypergeometric()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             title = paste0("p-value = ", vp_title), titlefont = list(size=15))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      if(!is.null(c_bait_in())){
        if(input$c_marker_text_inweb == "yes_label"){
          vp_layer_inweb <- vp_layer_for_inweb(p, c1_multi_vp$d_in)
        } else if(input$c_marker_text_inweb == "no_label"){
          vp_layer_inweb <- vp_layer_for_inweb_no_text(p, c1_multi_vp$d_in)
        }
        p <- vp_layer_inweb
      }
    } else if(input$colorscheme == "cbf"){
      if(!is.null(c_bait_in())){
        if(input$c_marker_text_inweb == "yes_label"){
          vp_layer_inweb <- vp_layer_for_inweb_cbf(p, c1_multi_vp$d_in)
        } else if(input$c_marker_text_inweb == "no_label"){
          vp_layer_inweb <- vp_layer_for_inweb_cbf_no_text(p, c1_multi_vp$d_in)
        }
        p <- vp_layer_inweb
      }
    }
    p <- p %>% 
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp1_inweb_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp1_inweb_layer()
    goi <- c_search_gene()
    orig_data <- c_pd1()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_goi_vp1 <- eventReactive(input$c_make_plot_goi, {
    if(!is.null(c_upload_genes())){
      d <- c_pd1()
      gene_interest <- c_genes_uploaded()
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )
      list(d_g2s=d_g2s)
    }
  })
    
  c_vp1_goi_layer <- reactive({
    validate(
      need(!is.null(c_pd1()), "")
    )
    d <- c_pd1()
    c1_goi <- c_goi_vp1()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    # vp_title <- c_pd3_inweb_hypergeometric()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      # p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p 
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p %>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             legend = list(orientation = 'h', y = -0.23))
             # title = paste0("p-value = ", vp_title), titlefont = list(size=15))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      if(!is.null(c_upload_genes())){
        df <- ldply(c1_goi$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$c_marker_text_goi == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes(p, df)
          } else if(input$c_marker_text_goi == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none(p, d)
          p <- vp_layer_no_genes
        }
      }
    } else if(input$colorscheme == "cbf"){
      if(!is.null(c_upload_genes())){
        df <- ldply(c1_goi$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$c_marker_text_goi == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf(p, df)
          } else if(input$c_marker_text_goi == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none_cbf(p, d)
          p <- vp_layer_no_genes
        }
      }
    }
    p <- p %>% 
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
    
  c_vp1_goi_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp1_goi_layer()
    goi <- c_search_gene()
    orig_data <- c_pd1()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_snp_vp1 <- eventReactive(input$c_make_plot_snp, {
    if(!is.null(c_snp())){
      d <- c_pd1()
      snp_interest <- c_SNP_to_gene()
      d_snp <- merge(d, snp_interest, by.x = 'gene', by.y = 'gene')
      list(d_snp=d_snp)
    }
  })
  
  c_vp1_snp_layer <- reactive({
    validate(
      need(!is.null(c_pd1()), "")
    )
    d <- c_pd1()
    c1_snp <- c_snp_vp1()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    # vp_title <- c_pd3_inweb_hypergeometric()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      # p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             legend = list(orientation = 'h', y = -0.23))
             #title = paste0("p-value = ", vp_title), titlefont = list(size=15))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      if(!is.null(c_snp())){
        if(nrow(c1_snp$d_snp) != 0){
          snp_sgl <- subset(c1_snp$d_snp, Freq == 1)
          snp_mgl <- subset(c1_snp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl(p, snp_sgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_no_text(p, snp_sgl)
            }
            p <- vp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl(p, snp_mgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_no_text(p, snp_mgl)
            }
            p <- vp_layer_snp2gene_mgl
          }
        } else{
          vp_layer_no_snp2gene <- vp_layer_for_snp_to_gene_none(p, d)
          p <- vp_layer_no_snp2gene
        }
      }
    } else if(input$colorscheme == "cbf"){
      if(!is.null(c_snp())){
        if(nrow(c1_snp$d_snp) != 0){
          snp_sgl <- subset(c1_snp$d_snp, Freq == 1)
          snp_mgl <- subset(c1_snp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_cbf(p, snp_sgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_cbf_no_text(p, snp_sgl)
            }
            p <- vp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_cbf(p, snp_mgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_cbf_no_text(p, snp_mgl)
            }
            p <- vp_layer_snp2gene_mgl
          }
        } else{
          vp_layer_no_snp2gene <- vp_layer_for_snp_to_gene_none_cbf(p, d)
          p <- vp_layer_no_snp2gene
        }
      }
    }
    p <- p %>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"),
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp1_snp_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp1_snp_layer()
    goi <- c_search_gene()
    orig_data <- c_pd1()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_vp2 <- reactive({
    d <- c_pd2()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    }
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    p <- p %>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp2_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp2()
    goi <- c_search_gene()
    orig_data <- c_pd2()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_vp2_inweb <- eventReactive(input$c_make_plot_inweb, {
    validate(
      need(!is.null(c_pd2()), "")
    )
    d <- c_pd2()
    if(!is.null(c_inweb_pd2())){
      inwebFile <- c_inweb_pd2()
      bait <- c_bait_in()
      d_in <- subset(d, gene %in% inwebFile$gene & gene != bait)
      list(d_in=d_in)
    }
  })
  
  c_vp2_inweb_layer <- reactive({
    validate(
      need(!is.null(c_pd2()), "")
    )
    d <- c_pd2()
    c2_multi_vp <- c_vp2_inweb()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    vp_title <- c_pd2_inweb_hypergeometric()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             title = paste0("p-value = ", vp_title), titlefont = list(size=15))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      if(!is.null(c_bait_in())){
        if(input$c_marker_text_inweb == "yes_label"){
          vp_layer_inweb <- vp_layer_for_inweb(p, c2_multi_vp$d_in)
        } else if(input$c_marker_text_inweb == "no_label"){
          vp_layer_inweb <- vp_layer_for_inweb_no_text(p, c2_multi_vp$d_in)
        }
        p <- vp_layer_inweb
      }
    } else if(input$colorscheme == "cbf"){
      if(!is.null(c_bait_in())){
        if(input$c_marker_text_inweb == "yes_label"){
          vp_layer_inweb <- vp_layer_for_inweb_cbf(p, c2_multi_vp$d_in)
        } else if(input$c_marker_text_inweb == "no_label"){
          vp_layer_inweb <- vp_layer_for_inweb_cbf_no_text(p, c2_multi_vp$d_in)
        }
        p <- vp_layer_inweb
      }
    }
    p <- p %>% 
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp2_inweb_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp2_inweb_layer()
    goi <- c_search_gene()
    orig_data <- c_pd2()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_goi_vp2 <- eventReactive(input$c_make_plot_goi, {
    if(!is.null(c_upload_genes())){
      d <- c_pd2()
      gene_interest <- c_genes_uploaded()
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )
      list(d_g2s=d_g2s)
    }
  })
  
  c_vp2_goi_layer <- reactive({
    validate(
      need(!is.null(c_pd2()), "")
    )
    d <- c_pd2()
    c2_goi <- c_goi_vp2()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    # vp_title <- c_pd3_inweb_hypergeometric()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      # p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F), 
             legend = list(orientation = 'h', y = -0.23))
    # title = paste0("p-value = ", vp_title), titlefont = list(size=15))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      if(!is.null(c_upload_genes())){
        df <- ldply(c2_goi$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$c_marker_text_goi == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes(p, df)
          } else if(input$c_marker_text_goi == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none(p, d)
          p <- vp_layer_no_genes
        }
      }
    } else if(input$colorscheme == "cbf"){
      if(!is.null(c_upload_genes())){
        df <- ldply(c2_goi$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$c_marker_text_goi == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf(p, df)
          } else if(input$c_marker_text_goi == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none_cbf(p, d)
          p <- vp_layer_no_genes
        }
      }
    }
    p <- p %>% 
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp2_goi_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp2_goi_layer()
    goi <- c_search_gene()
    orig_data <- c_pd2()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  
  c_snp_vp2 <- eventReactive(input$c_make_plot_snp, {
    if(!is.null(c_snp())){
      d <- c_pd2()
      snp_interest <- c_SNP_to_gene()
      d_snp <- merge(d, snp_interest, by.x = 'gene', by.y = 'gene')
      list(d_snp=d_snp)
    }
  })
  
  c_vp2_snp_layer <- reactive({
    validate(
      need(!is.null(c_pd2()), "")
    )
    d <- c_pd2()
    c2_snp <- c_snp_vp2()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    # vp_title <- c_pd3_inweb_hypergeometric()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      # p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             legend = list(orientation = 'h', y = -0.23))
    #title = paste0("p-value = ", vp_title), titlefont = list(size=15))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      if(!is.null(c_snp())){
        if(nrow(c2_snp$d_snp) != 0){
          snp_sgl <- subset(c2_snp$d_snp, Freq == 1)
          snp_mgl <- subset(c2_snp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl(p, snp_sgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_no_text(p, snp_sgl)
            }
            p <- vp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl(p, snp_mgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_no_text(p, snp_mgl)
            }
            p <- vp_layer_snp2gene_mgl
          }
        } else{
          vp_layer_no_snp2gene <- vp_layer_for_snp_to_gene_none(p, d)
          p <- vp_layer_no_snp2gene
        }
      }
    } else if(input$colorscheme == "cbf"){
      if(!is.null(c_snp())){
        if(nrow(c2_snp$d_snp) != 0){
          snp_sgl <- subset(c2_snp$d_snp, Freq == 1)
          snp_mgl <- subset(c2_snp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_cbf(p, snp_sgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_cbf_no_text(p, snp_sgl)
            }
            p <- vp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_cbf(p, snp_mgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_cbf_no_text(p, snp_mgl)
            }
            p <- vp_layer_snp2gene_mgl
          }
        } else{
          vp_layer_no_snp2gene <- vp_layer_for_snp_to_gene_none_cbf(p, d)
          p <- vp_layer_no_snp2gene
        }
      }
    }
    p <- p %>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"),
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp2_snp_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp2_snp_layer()
    goi <- c_search_gene()
    orig_data <- c_pd2()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_vp3 <- reactive({
    d <- c_pd3()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    }
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    p <- p %>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp3_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp3()
    goi <- c_search_gene()
    orig_data <- c_pd3()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_vp3_inweb <- eventReactive(input$c_make_plot_inweb, {
    validate(
      need(!is.null(c_pd3()), "")
    )
    d <- c_pd3()
    if(!is.null(c_inweb_pd3())){
      inwebFile <- c_inweb_pd3()
      bait <- c_bait_in()
      d_in <- subset(d, gene %in% inwebFile$gene & gene != bait)
      list(d_in=d_in)
    }
  })
  
  c_vp3_inweb_layer <- reactive({
    validate(
      need(!is.null(c_pd3()), "")
    )
    d <- c_pd3()
    c3_multi_vp <- c_vp3_inweb()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    vp_title <- c_pd3_inweb_hypergeometric()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_volcano_multiple_cond(data)
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             title = paste0("p-value = ", vp_title), titlefont = list(size=15))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      if(!is.null(c_bait_in())){
        if(input$c_marker_text_inweb == "yes_label"){
          vp_layer_inweb <- vp_layer_for_inweb(p, c3_multi_vp$d_in)
        }
        else if(input$c_marker_text_inweb == "no_label"){
          vp_layer_inweb <- vp_layer_for_inweb_no_text(p, c3_multi_vp$d_in)
        }
        p <- vp_layer_inweb
      }
    } else if(input$colorscheme == "cbf"){
      if(!is.null(c_bait_in())){
        if(input$c_marker_text_inweb == "yes_label"){
          vp_layer_inweb <- vp_layer_for_inweb_cbf(p, c3_multi_vp$d_in)
        } else if(input$c_marker_text_inweb == "no_label"){
          vp_layer_inweb <- vp_layer_for_inweb_cbf_no_text(p, c3_multi_vp$d_in)
        }
        p <- vp_layer_inweb
      }
    }
    p <- p %>% 
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp3_inweb_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp3_inweb_layer()
    goi <- c_search_gene()
    orig_data <- c_pd3()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_goi_vp3 <- eventReactive(input$c_make_plot_goi, {
    if(!is.null(c_upload_genes())){
      d <- c_pd3()
      gene_interest <- c_genes_uploaded()
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )
      list(d_g2s=d_g2s)
    }
  })
  
  c_vp3_goi_layer <- reactive({
    validate(
      need(!is.null(c_pd3()), "")
    )
    d <- c_pd3()
    c3_goi <- c_goi_vp3()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    # vp_title <- c_pd3_inweb_hypergeometric()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      # p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             legend = list(orientation = 'h', y = -0.23))
    # title = paste0("p-value = ", vp_title), titlefont = list(size=15))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      if(!is.null(c_upload_genes())){
        df <- ldply(c3_goi$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$c_marker_text_goi == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes(p, df)
          } else if(input$c_marker_text_goi == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none(p, d)
          p <- vp_layer_no_genes
        }
      }
    } else if(input$colorscheme == "cbf"){
      if(!is.null(c_upload_genes())){
        df <- ldply(c3_goi$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$c_marker_text_goi == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf(p, df)
          } else if(input$c_marker_text_goi == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none_cbf(p, d)
          p <- vp_layer_no_genes
        }
      }
    }
    p <- p %>% 
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp3_goi_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp3_goi_layer()
    goi <- c_search_gene()
    orig_data <- c_pd3()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_snp_vp3 <- eventReactive(input$c_make_plot_snp, {
    if(!is.null(c_snp())){
      d <- c_pd3()
      snp_interest <- c_SNP_to_gene()
      d_snp <- merge(d, snp_interest, by.x = 'gene', by.y = 'gene')
      list(d_snp=d_snp)
    }
  })
  
  c_vp3_snp_layer <- reactive({
    validate(
      need(!is.null(c_pd3()), "")
    )
    d <- c_pd3()
    c3_snp <- c_snp_vp3()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    # vp_title <- c_pd3_inweb_hypergeometric()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "RdPu", showlegend = T, width = 300, height = 390)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      # p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             legend = list(orientation = 'h', y = -0.23))
    #title = paste0("p-value = ", vp_title), titlefont = list(size=15))
    if(input$colorscheme == "fdr" | input$colorscheme == "exac"){
      if(!is.null(c_snp())){
        if(nrow(c3_snp$d_snp) != 0){
          snp_sgl <- subset(c3_snp$d_snp, Freq == 1)
          snp_mgl <- subset(c3_snp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl(p, snp_sgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_no_text(p, snp_sgl)
            }
            p <- vp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl(p, snp_mgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_no_text(p, snp_mgl)
            }
            p <- vp_layer_snp2gene_mgl
          }
        } else{
          vp_layer_no_snp2gene <- vp_layer_for_snp_to_gene_none(p, d)
          p <- vp_layer_no_snp2gene
        }
      }
    } else if(input$colorscheme == "cbf"){
      if(!is.null(c_snp())){
        if(nrow(c3_snp$d_snp) != 0){
          snp_sgl <- subset(c3_snp$d_snp, Freq == 1)
          snp_mgl <- subset(c3_snp$d_snp, Freq != 1)
          if(nrow(snp_sgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_cbf(p, snp_sgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_sgl <- vp_layer_for_snp_to_gene_sgl_cbf_no_text(p, snp_sgl)
            }
            p <- vp_layer_snp2gene_sgl
          }
          if(nrow(snp_mgl) != 0){
            if(input$c_marker_text_snp == "yes_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_cbf(p, snp_mgl)
            } else if(input$c_marker_text_snp == "no_label"){
              vp_layer_snp2gene_mgl <- vp_layer_for_snp_to_gene_mgl_cbf_no_text(p, snp_mgl)
            }
            p <- vp_layer_snp2gene_mgl
          }
        } else{
          vp_layer_no_snp2gene <- vp_layer_for_snp_to_gene_none_cbf(p, d)
          p <- vp_layer_no_snp2gene
        }
      }
    }
    p <- p %>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"),
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp3_snp_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_vp3_snp_layer()
    goi <- c_search_gene()
    orig_data <- c_pd3()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_sp1_cor <- reactive({
    input_file <- c_pd1()
    if("rep1" %in% colnames(input_file) & "rep2" %in% colnames(input_file)){
      cor <- signif(cor(input_file$rep1, input_file$rep2), 4)
      cor <- paste0("correlation coefficient: ", cor)
    } else if ("logFC" %in% colnames(input_file) & "FDR" %in% colnames(input_file) & "pvalue" %in% colnames(input_file)){
      return(NULL)
    }
  })
  
  c_sp1 <- reactive({
    d <- c_pd1()
    cc <- c_sp1_cor()
    if(input$c_colorscheme == "fdr"){
      d1 <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_scatter_multiple_cond(d, d1)
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_scatter_exac_multi(d, below_thresh, above_thresh, no_exist)
      p
    }
    if(input$c_colorscheme == "cbf"){
      d1 <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_scatter_multiple_cond(d, d1)
      p
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             title = cc, titlefont = list(size=12))
  })
  
  c_sp1_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_sp1()
    goi <- c_search_gene()
    orig_data <- c_pd1()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_scatter(p, searchgene)
    p1
  })
  
  c_sp2_cor <- reactive({
    input_file <- c_pd2()
    if("rep1" %in% colnames(input_file) & "rep2" %in% colnames(input_file)){
      cor <- signif(cor(input_file$rep1, input_file$rep2), 4)
      cor <- paste0("correlation coefficient: ", cor)
    } else if ("logFC" %in% colnames(input_file) & "FDR" %in% colnames(input_file) & "pvalue" %in% colnames(input_file)){
      return(NULL)
    }
  })
  
  c_sp2 <- reactive({
    d <- c_pd2()
    cc <- c_sp2_cor()
    if(input$c_colorscheme == "fdr"){
      d1 <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_scatter_multiple_cond(d, d1)
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_scatter_exac_multi(d, below_thresh, above_thresh, no_exist)
      p
    }
    if(input$c_colorscheme == "cbf"){
      d1 <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_scatter_multiple_cond(d, d1)
      p
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             title = cc, titlefont = list(size=12))
  })
  
  c_sp2_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_sp2()
    goi <- c_search_gene()
    orig_data <- c_pd2()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_scatter(p, searchgene)
    p1
  })
  
  c_sp3_cor <- reactive({
    input_file <- c_pd3()
    if("rep1" %in% colnames(input_file) & "rep2" %in% colnames(input_file)){
      cor <- signif(cor(input_file$rep1, input_file$rep2), 4)
      cor <- paste0("correlation coefficient: ", cor)
    } else if ("gene" %in% colnames(input_file) & "logFC" %in% colnames(input_file) 
             & "FDR" %in% colnames(input_file) & "pvalue" %in% colnames(input_file)){
      return(NULL)
    }
  })
  
  c_sp3 <- reactive({
    d <- c_pd3()
    cc <- c_sp3_cor()
    if(input$c_colorscheme == "fdr"){
      d1 <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_scatter_multiple_cond(d, d1)
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_scatter_exac_multi(d, below_thresh, above_thresh, no_exist)
      p
    }
    if(input$c_colorscheme == "cbf"){
      d1 <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_scatter_multiple_cond(d, d1)
      p
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(rep1, rep2))-1, (max(rep1, rep2))+1)), 
             title = cc, titlefont = list(size=12))
  })
  
  c_sp3_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_sp3()
    goi <- c_search_gene()
    orig_data <- c_pd3()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_scatter(p, searchgene)
    p1
  })
  
  c_f1 <- reactive({
    f1 <- c_pd1()
    f1_subset <- subset(f1, FDR < input$c_compare1_FDR_range[2] & FDR > input$c_compare1_FDR_range[1]
                        & pvalue < input$c_compare1_pvalue_range[2] & pvalue > input$c_compare1_pvalue_range[1]
                        & logFC < input$c_f1_logFC_range[2] & logFC > input$c_f1_logFC_range[1])
  })
  
  c_f2 <- reactive({
    f2 <- c_pd2()
    f2_subset <- subset(f2, FDR < input$c_compare2_FDR_range[2] & FDR > input$c_compare2_FDR_range[1]
                        & pvalue < input$c_compare2_pvalue_range[2] & pvalue > input$c_compare2_pvalue_range[1]
                        & logFC < input$c_f2_logFC_range[2] & logFC > input$c_f2_logFC_range[1])
  })
  
  c_f3 <- reactive({
    f3 <- c_pd3()
    f3_subset <- subset(f3, FDR < input$c_compare3_FDR_range[2] & FDR > input$c_compare3_FDR_range[1]
                        & pvalue < input$c_compare3_pvalue_range[2] & pvalue > input$c_compare3_pvalue_range[1]
                        & logFC < input$c_f3_logFC_range[2] & logFC > input$c_f3_logFC_range[1])
  })
  
  c_f1_pf <- reactive({
    f1 <- c_pd1()
    f1_subset <- subset(f1, FDR < input$c_f1_pf_FDR[2] & FDR > input$c_f1_pf_FDR[1]
                        & pvalue < input$c_f1_pf_pvalue[2] & pvalue > input$c_f1_pf_pvalue[1]
                        & logFC < input$c_f1_pf_logFC_range[2] & logFC > input$c_f1_pf_logFC_range[1])
  })
  
  c_f2_pf <- reactive({
    f2 <- c_pd2()
    f2_subset <- subset(f2, FDR < input$c_f2_pf_FDR[2] & FDR > input$c_f2_pf_FDR[1]
                        & pvalue < input$c_f2_pf_pvalue[2] & pvalue > input$c_f2_pf_pvalue[1]
                        & logFC < input$c_f2_pf_logFC_range[2] & logFC > input$c_f2_pf_logFC_range[1])
  })
  
  c_f3_pf <- reactive({
    f3 <- c_pd3()
    f3_subset <- subset(f3, FDR < input$c_f3_pf_FDR[2] & FDR > input$c_f3_pf_FDR[1]
                        & pvalue < input$c_f3_pf_pvalue[2] & pvalue > input$c_f3_pf_pvalue[1]
                        & logFC < input$c_f3_pf_logFC_range[2] & logFC > input$c_f3_pf_logFC_range[1])
  })
  
  #comparison for file1
  c_compare1 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1 <- c_pd1()
      f1_subset <- c_f1()
      f2_subset <- c_f2()
      overlap1_2 <- subset(f1_subset, gene %in% f2_subset$gene)
      f1_subset <- subset(f1_subset, gene %!in% (overlap1_2)$gene)
      list(f1=f1, f1_subset=f1_subset, overlap1_2=overlap1_2)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1 <- c_pd1()
      f1_subset <- c_f1()
      f2_subset <- c_f2()
      f3_subset <- c_f3()
      overlap1_2 <- subset(f1_subset, gene %in% f2_subset$gene)
      overlap1_3 <- subset(f1_subset, gene %in% f3_subset$gene)
      overlap1_2_3 <- subset(f1_subset, gene %in% f2_subset$gene & gene %in% f3_subset$gene)
      f1_subset <- subset(f1_subset, gene %!in% (overlap1_2)$gene & gene %!in% (overlap1_3)$gene & gene %!in% (overlap1_2_3)$gene)
      overlap1_2 <- subset(overlap1_2, gene %!in% (overlap1_2_3)$gene)
      overlap1_3 <- subset(overlap1_3, gene %!in% (overlap1_2_3)$gene)
      list(f1=f1, f1_subset=f1_subset, overlap1_2=overlap1_2, overlap1_3=overlap1_3, overlap1_2_3=overlap1_2_3)
    }
  })
  
  #comparison for file2
  c_compare2 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f2 <- c_pd2()
      f1_subset <- c_f1()
      f2_subset <- c_f2()
      overlap2_1 <- subset(f2_subset, gene %in% f1_subset$gene)
      f2_subset <- subset(f2_subset, gene %!in% (overlap2_1)$gene)
      list(f2=f2, f2_subset=f2_subset, overlap2_1=overlap2_1)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f2 <- c_pd2()
      f1_subset <- c_f1()
      f2_subset <- c_f2()
      f3_subset <- c_f3()
      overlap2_1 <- subset(f2_subset, gene %in% f1_subset$gene)
      overlap2_3 <- subset(f2_subset, gene %in% f3_subset$gene)
      overlap2_1_3 <- subset(f2_subset, gene %in% f1_subset$gene & gene %in% f3_subset$gene)
      f2_subset <- subset(f2_subset, gene %!in% (overlap2_1)$gene & gene %!in% (overlap2_3)$gene & gene %!in% (overlap2_1_3)$gene)
      overlap2_1 <- subset(overlap2_1, gene %!in% (overlap2_1_3)$gene)
      overlap2_3 <- subset(overlap2_3, gene %!in% (overlap2_1_3)$gene)
      list(f2=f2, f2_subset=f2_subset, overlap2_1=overlap2_1, overlap2_3=overlap2_3, overlap2_1_3=overlap2_1_3)
    }
  })
  
  #comparison for file3
  c_compare3 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f3 <- c_pd3()
      f1_subset <- c_f1()
      f2_subset <- c_f2()
      f3_subset <- c_f3()
      overlap3_1 <- subset(f3_subset, gene %in% f1_subset$gene)
      overlap3_2 <- subset(f3_subset, gene %in% f2_subset$gene)
      overlap3_1_2 <- subset(f3_subset, gene %in% f1_subset$gene & gene %in% f2_subset$gene)
      f3_subset <- subset(f3_subset, gene %!in% (overlap3_1)$gene & gene %!in% (overlap3_2)$gene & gene %!in% (overlap3_1_2)$gene)
      overlap3_1 <- subset(overlap3_1, gene %!in% (overlap3_1_2)$gene)
      overlap3_2 <- subset(overlap3_2, gene %!in% (overlap3_1_2)$gene)
      list(f3=f3, f3_subset=f3_subset, overlap3_1=overlap3_1, overlap3_2=overlap3_2, overlap3_1_2=overlap3_1_2)
    }
  })
  
  #comparison for file1
  c_compare1_pf <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1 <- c_pd1()
      f1_subset <- c_f1_pf()
      f2_subset <- c_f2_pf()
      overlap1_2 <- subset(f1_subset, gene %in% f2_subset$gene)
      f1_subset <- subset(f1_subset, gene %!in% (overlap1_2)$gene)
      list(f1=f1, f1_subset=f1_subset, overlap1_2=overlap1_2)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1 <- c_pd1()
      f1_subset <- c_f1_pf()
      f2_subset <- c_f2_pf()
      f3_subset <- c_f3_pf()
      overlap1_2 <- subset(f1_subset, gene %in% f2_subset$gene)
      overlap1_3 <- subset(f1_subset, gene %in% f3_subset$gene)
      overlap1_2_3 <- subset(f1_subset, gene %in% f2_subset$gene & gene %in% f3_subset$gene)
      f1_subset <- subset(f1_subset, gene %!in% (overlap1_2)$gene & gene %!in% (overlap1_3)$gene & gene %!in% (overlap1_2_3)$gene)
      overlap1_2 <- subset(overlap1_2, gene %!in% (overlap1_2_3)$gene)
      overlap1_3 <- subset(overlap1_3, gene %!in% (overlap1_2_3)$gene)
      list(f1=f1, f1_subset=f1_subset, overlap1_2=overlap1_2, overlap1_3=overlap1_3, overlap1_2_3=overlap1_2_3)
    }
  })
  
  #comparison for file2
  c_compare2_pf <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f2 <- c_pd2()
      f1_subset <- c_f1_pf()
      f2_subset <- c_f2_pf()
      overlap2_1 <- subset(f2_subset, gene %in% f1_subset$gene)
      f2_subset <- subset(f2_subset, gene %!in% (overlap2_1)$gene)
      list(f2=f2, f2_subset=f2_subset, overlap2_1=overlap2_1)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f2 <- c_pd2()
      f1_subset <- c_f1_pf()
      f2_subset <- c_f2_pf()
      f3_subset <- c_f3_pf()
      overlap2_1 <- subset(f2_subset, gene %in% f1_subset$gene)
      overlap2_3 <- subset(f2_subset, gene %in% f3_subset$gene)
      overlap2_1_3 <- subset(f2_subset, gene %in% f1_subset$gene & gene %in% f3_subset$gene)
      f2_subset <- subset(f2_subset, gene %!in% (overlap2_1)$gene & gene %!in% (overlap2_3)$gene & gene %!in% (overlap2_1_3)$gene)
      overlap2_1 <- subset(overlap2_1, gene %!in% (overlap2_1_3)$gene)
      overlap2_3 <- subset(overlap2_3, gene %!in% (overlap2_1_3)$gene)
      list(f2=f2, f2_subset=f2_subset, overlap2_1=overlap2_1, overlap2_3=overlap2_3, overlap2_1_3=overlap2_1_3)
    }
  })
  
  #comparison for file3
  c_compare3_pf <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f3 <- c_pd3()
      f1_subset <- c_f1_pf()
      f2_subset <- c_f2_pf()
      f3_subset <- c_f3_pf()
      overlap3_1 <- subset(f3_subset, gene %in% f1_subset$gene)
      overlap3_2 <- subset(f3_subset, gene %in% f2_subset$gene)
      overlap3_1_2 <- subset(f3_subset, gene %in% f1_subset$gene & gene %in% f2_subset$gene)
      f3_subset <- subset(f3_subset, gene %!in% (overlap3_1)$gene & gene %!in% (overlap3_2)$gene & gene %!in% (overlap3_1_2)$gene)
      overlap3_1 <- subset(overlap3_1, gene %!in% (overlap3_1_2)$gene)
      overlap3_2 <- subset(overlap3_2, gene %!in% (overlap3_1_2)$gene)
      list(f3=f3, f3_subset=f3_subset, overlap3_1=overlap3_1, overlap3_2=overlap3_2, overlap3_1_2=overlap3_1_2)
    }
  })
  
  c_compare1_plot <- reactive({
    d <- c_compare1()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      p1 <- compare_two_files_a(d$f1, d$f1_subset, d$overlap1_2)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      p1 <- compare_two_files_aa(d$f1, d$f1_subset, d$overlap1_2, d$overlap1_3, d$overlap1_2_3)
    }
  })
  
  c_compare1_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_compare1_plot()
    goi <- c_search_gene()
    orig_data <- c_pd1()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p1 <- search_volcano(p, searchgene)
    p1
  })
  
  c_compare2_plot <- reactive({
    d <- c_compare2()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      p2 <- compare_two_files_b(d$f2, d$f2_subset, d$overlap2_1)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      p2 <- compare_two_files_bb(d$f2, d$f2_subset, d$overlap2_1, d$overlap2_3, d$overlap2_1_3)
    }
  })
  
  c_compare2_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_compare2_plot()
    goi <- c_search_gene()
    orig_data <- c_pd2()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p2 <- search_volcano(p, searchgene)
    p2
  })

  c_compare3_plot <- reactive({
    d <- c_compare3()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      p3 <- compare_two_files_cc(d$f3, d$f3_subset, d$overlap3_1, d$overlap3_2, d$overlap3_1_2)
    }
  })
  
  c_compare3_plus <- reactive({
    validate(
      need(!is.null(c_search_gene()), "")
    )
    p <- c_compare3_plot()
    goi <- c_search_gene()
    orig_data <- c_pd3()
    searchgene <- orig_data[grepl(goi,orig_data$gene),]
    p3 <- search_volcano(p, searchgene)
    p3
  })
  
  c_compare1_pfe_plot <- eventReactive(input$c_make_bpf, {
    d <- c_compare1_pf()
    pfmsizing <- c_PF_marker()
    pfsort <- c_PF_sorting()
    increase_size <- input$c_PF_marker_freq
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      withProgress(message = 'This may take a while', 
                   detail = 'Hold please', value = 0, {
                     pf_f1 <- d$f1
                     pf_f1s <- d$f1_subset
                     pf_overlap1_2 <- d$overlap1_2
                     incProgress(0.2)
                     #f1 subset
                     f1s_families <- assignFamily_inc_doubles(pf_f1s)
                     f1s_gna <- addNames(pf_f1s)
                     incProgress(0.5)
                     #f1 and f2 overlap
                     o12_families <- assignFamily_inc_doubles(pf_overlap1_2)
                     o12_gna <- addNames(pf_overlap1_2)
                     incProgress(0.8)
                     f1_subset <- makePlotFamilies_1quadrant(f1s_families, f1s_gna, pf_f1, pfsort)
                     overlap1_2 <- makePlotFamilies_1quadrant(o12_families, o12_gna, pf_f1, pfsort)
                     incProgress(0.9)
                     if (pfmsizing == "change"){
                       p1 <- compare_two_files_pf_a_size(pf_f1, f1_subset[[1]], f1_subset[[2]], overlap1_2[[1]], overlap1_2[[2]], increase_size)
                     } else if (pfmsizing == "no_change"){
                       p1 <- compare_two_files_pf_a(pf_f1, f1_subset[[1]], f1_subset[[2]], overlap1_2[[1]], overlap1_2[[2]])
                     }
                   })
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      withProgress(message = 'This may take a while', 
                   detail = 'Hold please', value = 0, {
                     pf_f1 <- d$f1
                     pf_f1s <- d$f1_subset
                     pf_overlap1_2 <- d$overlap1_2
                     pf_overlap1_3 <- d$overlap1_3
                     pf_overlap1_2_3 <- d$overlap1_2_3
                     incProgress(0.1)
                     #f1 subset
                     f1s_families <- assignFamily_inc_doubles(pf_f1s)
                     f1s_gna <- addNames(pf_f1s)
                     incProgress(0.3)
                     #f1 and f2 overlap
                     o12_families <- assignFamily_inc_doubles(pf_overlap1_2)
                     o12_gna <- addNames(pf_overlap1_2)
                     incProgress(0.5)
                     #f1 and f3 overlap
                     o13_families <- assignFamily_inc_doubles(pf_overlap1_3)
                     o13_gna <- addNames(pf_overlap1_3)
                     incProgress(0.7)
                     #f1 and f2 overlap
                     o123_families <- assignFamily_inc_doubles(pf_overlap1_2_3)
                     o123_gna <- addNames(pf_overlap1_2_3)
                     incProgress(0.9)
                     f1_subset <- makePlotFamilies_1quadrant(f1s_families, f1s_gna, pf_f1, pfsort)
                     overlap1_2 <- makePlotFamilies_1quadrant(o12_families, o12_gna, pf_f1, pfsort)
                     overlap1_3 <- makePlotFamilies_1quadrant(o13_families, o13_gna, pf_f1, pfsort)
                     overlap1_2_3 <- makePlotFamilies_1quadrant(o123_families, o123_gna, pf_f1, pfsort)
                     
                     if (pfmsizing == "change"){
                       p1 <- compare_two_files_pf_aa_size(pf_f1, f1_subset[[1]], f1_subset[[2]], overlap1_2[[1]], overlap1_2[[2]],
                                                          overlap1_3[[1]], overlap1_3[[2]], overlap1_2_3[[1]], overlap1_2_3[[2]], increase_size)
                     } else{
                       p1 <- compare_two_files_pf_aa(pf_f1, f1_subset[[1]], f1_subset[[2]], overlap1_2[[1]], overlap1_2[[2]],
                                                     overlap1_3[[1]], overlap1_3[[2]], overlap1_2_3[[1]], overlap1_2_3[[2]])
                     }
                   })
    }
  })
  
  c_compare2_pfe_plot <- eventReactive(input$c_make_bpf, {
    d <- c_compare2_pf()
    pfmsizing <- c_PF_marker()
    pfsort <- c_PF_sorting()
    increase_size <- input$c_PF_marker_freq
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      withProgress(message = 'This may take a while', 
                   detail = 'Hold please', value = 0, {
                     pf_f2 <- d$f2
                     pf_f2s <- d$f2_subset
                     pf_overlap2_1 <- d$overlap2_1
                     incProgress(0.2)
                     #f2 subset
                     f2s_families <- assignFamily_inc_doubles(pf_f2s)
                     f2s_gna <- addNames(pf_f2s)
                     incProgress(0.5)
                     #f1 and f2 overlap
                     o21_families <- assignFamily_inc_doubles(pf_overlap2_1)
                     o21_gna <- addNames(pf_overlap2_1)
                     incProgress(0.8)
                     f2_subset <- makePlotFamilies_1quadrant(f2s_families, f2s_gna, pf_f2, pfsort)
                     overlap2_1 <- makePlotFamilies_1quadrant(o21_families, o21_gna, pf_f2, pfsort)
                     incProgress(0.9)
                     if (pfmsizing == "change"){
                       p1 <- compare_two_files_pf_b_size(pf_f2, f2_subset[[1]], f2_subset[[2]], overlap2_1[[1]], overlap2_1[[2]], increase_size)
                     } else if (pfmsizing == "no_change"){
                       p1 <- compare_two_files_pf_b(pf_f2, f2_subset[[1]], f2_subset[[2]], overlap2_1[[1]], overlap2_1[[2]])
                     }
                   })
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      withProgress(message = 'This may take a while', 
                   detail = 'Hold please', value = 0, {
                     pf_f2 <- d$f2
                     pf_f2s <- d$f2_subset
                     pf_overlap2_1 <- d$overlap2_1
                     pf_overlap2_3 <- d$overlap2_3
                     pf_overlap2_1_3 <- d$overlap2_1_3
                     incProgress(0.1)
                     #f2 subset
                     f2s_families <- assignFamily_inc_doubles(pf_f2s)
                     f2s_gna <- addNames(pf_f2s)
                     incProgress(0.3)
                     #f1 and f2 overlap
                     o21_families <- assignFamily_inc_doubles(pf_overlap2_1)
                     o21_gna <- addNames(pf_overlap2_1)
                     incProgress(0.5)
                     #f1 and f3 overlap
                     o23_families <- assignFamily_inc_doubles(pf_overlap2_3)
                     o23_gna <- addNames(pf_overlap2_3)
                     incProgress(0.7)
                     #f1, f2 and f3 overlap
                     o213_families <- assignFamily_inc_doubles(pf_overlap2_1_3)
                     o213_gna <- addNames(pf_overlap2_1_3)
                     incProgress(0.9)
                     f2_subset <- makePlotFamilies_1quadrant(f2s_families, f2s_gna, pf_f2, pfsort)
                     overlap2_1 <- makePlotFamilies_1quadrant(o21_families, o21_gna, pf_f2, pfsort)
                     overlap2_3 <- makePlotFamilies_1quadrant(o23_families, o23_gna, pf_f2, pfsort)
                     overlap2_1_3 <- makePlotFamilies_1quadrant(o213_families, o213_gna, pf_f2, pfsort)
                     
                     if (pfmsizing == "change"){
                       p1 <- compare_two_files_pf_bb_size(pf_f2, f2_subset[[1]], f2_subset[[2]], overlap2_1[[1]], overlap2_1[[2]],
                                                          overlap2_3[[1]], overlap2_3[[2]], overlap2_1_3[[1]], overlap2_1_3[[2]], increase_size)
                     } else{
                       p1 <- compare_two_files_pf_bb(pf_f2, f2_subset[[1]], f2_subset[[2]], overlap2_1[[1]], overlap2_1[[2]],
                                                     overlap2_3[[1]], overlap2_3[[2]], overlap2_1_3[[1]], overlap2_1_3[[2]])
                     }
                   })
    }
  })
  
  c_compare3_pfe_plot <- eventReactive(input$c_make_bpf, {
    d <- c_compare3_pf()
    pfmsizing <- c_PF_marker()
    pfsort <- c_PF_sorting()
    increase_size <- input$c_PF_marker_freq
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      withProgress(message = 'This may take a while', 
                   detail = 'Hold please', value = 0, {
                     pf_f3 <- d$f3
                     pf_f3s <- d$f3_subset
                     pf_overlap3_1 <- d$overlap3_1
                     pf_overlap3_2 <- d$overlap3_2
                     pf_overlap3_1_2 <- d$overlap3_1_2
                     incProgress(0.1)
                     #f2 subset
                     f3s_families <- assignFamily_inc_doubles(pf_f3s)
                     f3s_gna <- addNames(pf_f3s)
                     incProgress(0.3)
                     #f1 and f2 overlap
                     o31_families <- assignFamily_inc_doubles(pf_overlap3_1)
                     o31_gna <- addNames(pf_overlap3_1)
                     incProgress(0.5)
                     #f1 and f3 overlap
                     o32_families <- assignFamily_inc_doubles(pf_overlap3_2)
                     o32_gna <- addNames(pf_overlap3_2)
                     incProgress(0.7)
                     #f1, f2 and f3 overlap
                     o312_families <- assignFamily_inc_doubles(pf_overlap3_1_2)
                     o312_gna <- addNames(pf_overlap3_1_2)
                     incProgress(0.9)
                     f3_subset <- makePlotFamilies_1quadrant(f3s_families, f3s_gna, pf_f3, pfsort)
                     overlap3_1 <- makePlotFamilies_1quadrant(o31_families, o31_gna, pf_f3, pfsort)
                     overlap3_2 <- makePlotFamilies_1quadrant(o32_families, o32_gna, pf_f3, pfsort)
                     overlap3_1_2 <- makePlotFamilies_1quadrant(o312_families, o312_gna, pf_f3, pfsort)
                     
                     if (pfmsizing == "change"){
                       p1 <- compare_two_files_pf_cc_size(pf_f3, f3_subset[[1]], f3_subset[[2]], overlap3_1[[1]], overlap3_1[[2]],
                                                          overlap3_2[[1]], overlap3_2[[2]], overlap3_1_2[[1]], overlap3_1_2[[2]], increase_size)
                     } else{
                       p1 <- compare_two_files_pf_cc(pf_f3, f3_subset[[1]], f3_subset[[2]], overlap3_1[[1]], overlap3_1[[2]],
                                                     overlap3_2[[1]], overlap3_2[[2]], overlap3_1_2[[1]], overlap3_1_2[[2]])
                     }
                   })
    }
  })
  
  c_f1_unique <- reactive({
    d <- c_compare1()
    f1 <- d$f1_subset
    d1 <- data.frame(f1$gene)
    colnames(d1) <- c("f1")    
    d1 <- unique(sort(d1$f1))
  })
  
  c_f2_unique <- reactive({
    d <- c_compare2()
    f2 <- d$f2_subset
    d1 <- data.frame(f2$gene)
    colnames(d1) <- c("f2")    
    d1 <- unique(sort(d1$f2))
  })
  
  c_f1_and_f2 <- reactive({
    d <- c_compare2()
    f2 <- d$overlap2_1
    d1 <- data.frame(f2$gene)
    colnames(d1) <- c("f12")    
    d1 <- unique(sort(d1$f12))
  })
  
  c_f3_unique <- reactive({
    d <- c_compare3()
    f3 <- d$f3_subset
    d1 <- data.frame(f3$gene)
    colnames(d1) <- c("f3")    
    d1 <- unique(sort(d1$f3))
  })
  
  c_f1_and_f3 <- reactive({
    d <- c_compare3()
    f3 <- d$overlap3_1
    d1 <- data.frame(f3$gene)
    colnames(d1) <- c("f13")    
    d1 <- unique(sort(d1$f13))
  })
  
  c_f2_and_f3 <- reactive({
    d <- c_compare3()
    f3 <- d$overlap3_2
    d1 <- data.frame(f3$gene)
    colnames(d1) <- c("f23")    
    d1 <- unique(sort(d1$f23))
  })
  
  c_f1_f2_and_f3 <- reactive({
    d <- c_compare3()
    d1 <- data.frame((d$overlap3_1_2)$gene)
    colnames(d1) <- c("f123")    
    d1 <- unique(sort(d1$f123))
  })
  
  c_unique_dat <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1 <- c_f1_unique()
      f2 <- c_f2_unique()
      f12 <- c_f1_and_f2()
      n <- max(length(f1), length(f2), length(f12))
      length(f1) <- n
      length(f2) <- n
      length(f12) <- n
      d <- cbind(f1, f2, f12)
      d[is.na(d)] <- " "
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1 <- c_f1_unique()
      f2 <- c_f2_unique()
      f12 <- c_f1_and_f2()
      f3 <- c_f3_unique()
      f13 <- c_f1_and_f3()
      f23 <- c_f2_and_f3()
      f123 <- c_f1_f2_and_f3()
      n <- max(length(f1), length(f2), length(f12), length(f3), length(f13), length(f23), length(f123))
      length(f1) <- n
      length(f2) <- n
      length(f12) <- n
      length(f3) <- n
      length(f13) <- n
      length(f23) <- n
      length(f123) <- n
      d <- cbind(f1, f2, f12, f3, f13, f23, f123)
      d[is.na(d)] <- " "
    }
    as.data.frame(d)
  })

  c_venndiagram <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1_subset <- c_f1()
      f2_subset <- c_f2()
      x <- list()
      x[["File1"]] <- f1_subset$gene
      x[["File2"]] <- f2_subset$gene
      
      colors2 = c("#e41a1c", "#ffff33")
      
      v0 <- venn.diagram(x,
                         fill = colors2, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                         sub = " ", sub.pos = c(0, 0), euler.d = F, scaled = F,
                         cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05),
                         fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1_subset <- c_f1()
      f2_subset <- c_f2()
      f3_subset <- c_f3()
      x <- list()
      x[["File1"]] <- f1_subset$gene
      x[["File2"]] <- f2_subset$gene
      x[["File3"]] <- f3_subset$gene
      
      colors3 = c("#e41a1c", "#ffff33", "#1f78b4")
      
      v0 <- venn.diagram(x, 
                         fill = colors3, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                         sub = " ", sub.pos = c(0, 0, 0), euler.d = F, scaled = F,
                         # cat.cex = 1.1, cex = 2, cat.pos = c(180,180,0), cat.dist = c(0.05,0.05,0.05),
                         fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
    }
    v0
  })
  
  #comparison for file1 inweb
  c_inweb_compare1 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1_inweb <- c_inweb_pd1()
      f1_inweb <- subset(f1_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f2_inweb <- c_inweb_pd2()
      f2_inweb <- subset(f2_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      overlap1_2 <- subset(f1_inweb, gene %in% f2_inweb$gene)
      list(f1_inweb=f1_inweb, overlap1_2=overlap1_2)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1_inweb <- c_inweb_pd1()
      f1_inweb <- subset(f1_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f2_inweb <- c_inweb_pd2()
      f2_inweb <- subset(f2_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f3_inweb <- c_inweb_pd3()
      f3_inweb <- subset(f3_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      overlap1_2 <- subset(f1_inweb, gene %in% f2_inweb$gene)
      overlap1_3 <- subset(f1_inweb, gene %in% f3_inweb$gene)
      overlap1_2_3 <- subset(f1_inweb, gene %in% f2_inweb$gene & gene %in% f3_inweb$gene)
      list(f1_inweb=f1_inweb, overlap1_2=overlap1_2, overlap1_3=overlap1_3, overlap1_2_3=overlap1_2_3)
    }
  })
  
  #comparison for file2 inweb
  c_inweb_compare2 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1_inweb <- c_inweb_pd1()
      f1_inweb <- subset(f1_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f2_inweb <- c_inweb_pd2()
      f2_inweb <- subset(f2_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      overlap2_1 <- subset(f2_inweb, gene %in% f1_inweb$gene)
      list(f2_inweb=f2_inweb, overlap2_1=overlap2_1)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1_inweb <- c_inweb_pd1()
      f1_inweb <- subset(f1_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f2_inweb <- c_inweb_pd2()
      f2_inweb <- subset(f2_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f3_inweb <- c_inweb_pd3()
      f3_inweb <- subset(f3_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      overlap2_1 <- subset(f2_inweb, gene %in% f1_inweb$gene)
      overlap2_3 <- subset(f2_inweb, gene %in% f3_inweb$gene)
      overlap2_1_3 <- subset(f2_inweb, gene %in% f1_inweb$gene & gene %in% f3_inweb$gene)
      list(f2_inweb=f2_inweb, overlap2_1=overlap2_1, overlap2_3=overlap2_3, overlap2_1_3=overlap2_1_3)
    }
  })
  
  #comparison for file3 inweb
  c_inweb_compare3 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1_inweb <- c_inweb_pd1()
      f1_inweb <- subset(f1_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f2_inweb <- c_inweb_pd2()
      f2_inweb <- subset(f2_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f3_inweb <- c_inweb_pd3()
      f3_inweb <- subset(f3_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      overlap3_1 <- subset(f3_inweb, gene %in% f1_inweb$gene)
      overlap3_2 <- subset(f3_inweb, gene %in% f2_inweb$gene)
      overlap3_1_2 <- subset(f3_inweb, gene %in% f1_inweb$gene & gene %in% f2_inweb$gene)
      list(f3_inweb=f3_inweb, overlap3_1=overlap3_1, overlap3_2=overlap3_2, overlap3_1_2=overlap3_1_2)
    }
  })
  
  c_f1_unique_inweb <- reactive({
    d <- c_inweb_compare1()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1 <- subset(d$f1_inweb, gene %!in% (d$overlap1_2)$gene)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1 <- subset(d$f1_inweb, gene %!in% (d$overlap1_2)$gene & gene %!in% (d$overlap1_3)$gene & gene %!in% (d$overlap1_2_3)$gene)
    }
    d1 <- data.frame(f1$gene)
    colnames(d1) <- c("f1")    
    d1 <- unique(sort(d1$f1))
  })
  
  c_f2_unique_inweb <- reactive({
    d <- c_inweb_compare2()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f2 <- subset(d$f2_inweb, gene %!in% (d$overlap2_1)$gene)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f2 <- subset(d$f2_inweb, gene %!in% (d$overlap2_1)$gene & gene %!in% (d$overlap2_3)$gene & gene %!in% (d$overlap2_1_3)$gene)
    }
    d1 <- data.frame(f2$gene)
    colnames(d1) <- c("f2")    
    d1 <- unique(sort(d1$f2))
  })
  
  c_f1_and_f2_inweb <- reactive({
    d <- c_inweb_compare2()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f2 <- d$overlap2_1
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f2 <- subset(d$overlap2_1, gene %!in% (d$overlap2_1_3)$gene)
    }
    d1 <- data.frame(f2$gene)
    colnames(d1) <- c("f12")    
    d1 <- unique(sort(d1$f12))
  })
  
  c_f3_unique_inweb <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_inweb_compare3()
      f3 <- subset(d$f3_inweb, gene %!in% (d$overlap3_1)$gene & gene %!in% (d$overlap3_2)$gene & gene %!in% (d$overlap3_1_2)$gene)
      d1 <- data.frame(f3$gene)
      colnames(d1) <- c("f3")    
      d1 <- unique(sort(d1$f3))
    }
  })
  
  c_f1_and_f3_inweb <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_inweb_compare3()
      f3 <- subset(d$overlap3_1, gene %!in% (d$overlap3_1_2)$gene)
      d1 <- data.frame(f3$gene)
      colnames(d1) <- c("f13")    
      d1 <- unique(sort(d1$f13))
    }
  })
  
  c_f2_and_f3_inweb <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_inweb_compare3()
      f3 <- subset(d$overlap3_2, gene %!in% (d$overlap3_1_2)$gene)
      d1 <- data.frame(f3$gene)
      colnames(d1) <- c("f23")    
      d1 <- unique(sort(d1$f23))
    }
  })
  
  c_f1_f2_and_f3_inweb <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_inweb_compare3()
      d1 <- data.frame((d$overlap3_1_2)$gene)
      colnames(d1) <- c("f123")    
      d1 <- unique(sort(d1$f123))
    }
  })
  
  c_unique_inweb_dat <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1 <- c_f1_unique_inweb()
      f2 <- c_f2_unique_inweb()
      f12 <- c_f1_and_f2_inweb()
      n <- max(length(f1), length(f2), length(f12))
      length(f1) <- n
      length(f2) <- n
      length(f12) <- n
      d <- cbind(f1, f2, f12)
      d[is.na(d)] <- " "
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1 <- c_f1_unique_inweb()
      f2 <- c_f2_unique_inweb()
      f12 <- c_f1_and_f2_inweb()
      f3 <- c_f3_unique_inweb()
      f13 <- c_f1_and_f3_inweb()
      f23 <- c_f2_and_f3_inweb()
      f123 <- c_f1_f2_and_f3_inweb()
      n <- max(length(f1), length(f2), length(f12), length(f3), length(f13), length(f23), length(f123))
      length(f1) <- n
      length(f2) <- n
      length(f12) <- n
      length(f3) <- n
      length(f13) <- n
      length(f23) <- n
      length(f123) <- n
      d <- cbind(f1, f2, f12, f3, f13, f23, f123)
      d[is.na(d)] <- " "
    }
    as.data.frame(d)
  })
  
  c_venndiagram_inweb <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      if(!is.null(c_inweb_pd1()) & !is.null(c_in_pd2())){
        f1_inweb <- c_inweb_pd1()
        f1_inweb <- subset(f1_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
        f2_inweb <- c_inweb_pd2()
        f2_inweb <- subset(f2_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
        x <- list()
        x[["File1"]] <- f1_inweb$gene
        x[["File2"]] <- f2_inweb$gene
        
        colors2 = c("gray", "gray")
        
        v0 <- venn.diagram(x,
                           fill = colors2, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                           sub = " ", sub.pos = c(0, 0), euler.d = F, scaled = F,
                           cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05),
                           fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
      }
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      if(!is.null(c_inweb_pd1()) & !is.null(c_in_pd2()) & !is.null(c_in_pd3())){
      f1_inweb <- c_inweb_pd1()
      f1_inweb <- subset(f1_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f2_inweb <- c_inweb_pd2()
      f2_inweb <- subset(f2_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      f3_inweb <- c_inweb_pd3()
      f3_inweb <- subset(f3_inweb, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
      x <- list()
      x[["File1"]] <- f1_inweb$gene
      x[["File2"]] <- f2_inweb$gene
      x[["File3"]] <- f3_inweb$gene
      
      colors3 = c("gray", "gray", "gray")
      
      v0 <- venn.diagram(x, 
                         fill = colors3, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                         sub = " ", sub.pos = c(0, 0, 0), euler.d = F, scaled = F,
                         # cat.cex = 1.1, cex = 2, cat.pos = c(180,180,0), cat.dist = c(0.05,0.05,0.05),
                         fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
      }
    }
  })
  
  c_pd1_inweb_hypergeometric <- reactive({
    d <- c_pd1()
    inweb <- c_inweb_pd1()
    pop <- subset(d, d$gene %in% inweb_combined$V1)
    sample <- subset(pop, pop$gene %in% inweb$gene)
    success_pop <- subset(pop, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
    success_samp <- subset(sample, sample$gene %in% success_pop$gene)
    rownames(success_samp) <- NULL
    samp_l <- nrow(sample)
    pop_l <- nrow(pop)
    success_pop_l <- nrow(success_pop)
    success_samp_l <- nrow(success_samp)
    pvalue <- phyper((success_samp_l-1), samp_l, (pop_l-samp_l), success_pop_l, lower.tail = F)
    pvalue <- signif(pvalue, 4)
  })
  
  c_pd2_inweb_hypergeometric <- reactive({
    d <- c_pd2()
    inweb <- c_inweb_pd2()
    pop <- subset(d, d$gene %in% inweb_combined$V1)
    sample <- subset(pop, pop$gene %in% inweb$gene)
    success_pop <- subset(pop, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
    success_samp <- subset(sample, sample$gene %in% success_pop$gene)
    rownames(success_samp) <- NULL
    samp_l <- nrow(sample)
    pop_l <- nrow(pop)
    success_pop_l <- nrow(success_pop)
    success_samp_l <- nrow(success_samp)
    pvalue <- phyper((success_samp_l-1), samp_l, (pop_l-samp_l), success_pop_l, lower.tail = F)
    pvalue <- signif(pvalue, 4)
  })
  
  c_pd3_inweb_hypergeometric <- reactive({
    d <- c_pd3()
    inweb <- c_inweb_pd3()
    pop <- subset(d, d$gene %in% inweb_combined$V1)
    sample <- subset(pop, pop$gene %in% inweb$gene)
    success_pop <- subset(pop, pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb)
    success_samp <- subset(sample, sample$gene %in% success_pop$gene)
    rownames(success_samp) <- NULL
    samp_l <- nrow(sample)
    pop_l <- nrow(pop)
    success_pop_l <- nrow(success_pop)
    success_samp_l <- nrow(success_samp)
    pvalue <- phyper((success_samp_l-1), samp_l, (pop_l-samp_l), success_pop_l, lower.tail = F)
    pvalue <- signif(pvalue, 4)
  })
  
  c_venndiagram_goi <- reactive({
    validate(
      need(!is.null(c_genes_uploaded()), ""),
      need(!is.null(c_vp1_goi_layer()), ""),
      need(!is.null(c_vp2_goi_layer()), "")
    )
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      if(!is.null(c_pd1()) & !is.null(c_pd2())){
        c1 <- c_pd1()
        c2 <- c_pd2()
        gene_interest <- c_genes_uploaded()
        df1_c1 <- lapply(gene_interest, function(x) subset(c1, gene %in% x & 
                                                             pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
        df1_c2 <- lapply(gene_interest, function(x) subset(c2, gene %in% x & 
                                                             pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
        
        total1 <- as.data.frame(data.table::rbindlist(df1_c1))
        total2 <- as.data.frame(data.table::rbindlist(df1_c2))
        
        df1_c1 <- c(df1_c1, list(total = total1))
        df1_c2 <- c(df1_c2, list(total = total2))
        
        list_num <- input$c_goi_num_inputs

        x <- list()
        x[["File1"]] <-df1_c1[[list_num]]$gene
        x[["File2"]] <-df1_c2[[list_num]]$gene
        
        vd_title <- list_num
        colors2 = c("gray", "gray")
        
        v0 <- venn.diagram(x,
                           fill = colors2, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                           sub = " ", sub.pos = c(0, 0), euler.d = F, scaled = F, main = vd_title,
                           cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05),
                           fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
      }
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      if(!is.null(c_pd1()) & !is.null(c_pd2()) & !is.null(c_pd3())){
        c1 <- c_pd1()
        c2 <- c_pd2()
        c3 <- c_pd3()
        gene_interest <- c_genes_uploaded()
        df1_c1 <- lapply(gene_interest, function(x) subset(c1, gene %in% x & 
                                                             pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
        df1_c2 <- lapply(gene_interest, function(x) subset(c2, gene %in% x & 
                                                             pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
        df1_c3 <- lapply(gene_interest, function(x) subset(c3, gene %in% x & 
                                                             pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
        
        total1 <- as.data.frame(data.table::rbindlist(df1_c1))
        total2 <- as.data.frame(data.table::rbindlist(df1_c2))
        total3 <- as.data.frame(data.table::rbindlist(df1_c3))
        
        df1_c1 <- c(df1_c1, list(total = total1))
        df1_c2 <- c(df1_c2, list(total = total2))
        df1_c3 <- c(df1_c3, list(total = total3))
        
        list_num <- input$c_goi_num_inputs
        
        x <- list()
        x[["File1"]] <-df1_c1[[list_num]]$gene
        x[["File2"]] <-df1_c2[[list_num]]$gene
        x[["File3"]] <-df1_c3[[list_num]]$gene
        
        vd_title <- list_num
        colors3 = c("gray", "gray", "gray")
        
        v0 <- venn.diagram(x, 
                           fill = colors3, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                           sub = " ", sub.pos = c(0, 0, 0), euler.d = F, scaled = F, main = vd_title,
                           # cat.cex = 1.1, cex = 2, cat.pos = c(180,180,0), cat.dist = c(0.05,0.05,0.05),
                           fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
      }
    }
  })
  
  #comparison for file1 goi
  c_goi_compare1 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      gene_interest <- c_genes_uploaded()
      df1_c1 <- lapply(gene_interest, function(x) subset(c1, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(gene_interest, function(x) subset(c2, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      
      list_num <- input$c_goi_num_inputs
      f1_goi <-df1_c1[[list_num]]
      f2_goi <-df1_c2[[list_num]]
      overlap1_2 <- subset(f1_goi, gene %in% f2_goi$gene)
      list(f1_goi=f1_goi, overlap1_2=overlap1_2)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      gene_interest <- c_genes_uploaded()
      df1_c1 <- lapply(gene_interest, function(x) subset(c1, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(gene_interest, function(x) subset(c2, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c3 <- lapply(gene_interest, function(x) subset(c3, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      total3 <- as.data.frame(data.table::rbindlist(df1_c3))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      df1_c3 <- c(df1_c3, list(total = total3))
      
      list_num <- input$c_goi_num_inputs
      f1_goi <-df1_c1[[list_num]]
      f2_goi <-df1_c2[[list_num]]
      f3_goi <-df1_c3[[list_num]]
      overlap1_2 <- subset(f1_goi, gene %in% f2_goi$gene)
      overlap1_3 <- subset(f1_goi, gene %in% f3_goi$gene)
      overlap1_2_3 <- subset(f1_goi, gene %in% f2_goi$gene & gene %in% f3_goi$gene)
      list(f1_goi=f1_goi, overlap1_2=overlap1_2, overlap1_3=overlap1_3, overlap1_2_3=overlap1_2_3)
    }
  })
  
  #comparison for file2 goi
  c_goi_compare2 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      gene_interest <- c_genes_uploaded()
      df1_c1 <- lapply(gene_interest, function(x) subset(c1, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(gene_interest, function(x) subset(c2, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      
      list_num <- input$c_goi_num_inputs
      f1_goi <-df1_c1[[list_num]]
      f2_goi <-df1_c2[[list_num]]
      overlap2_1 <- subset(f2_goi, gene %in% f1_goi$gene)
      list(f2_goi=f2_goi, overlap2_1=overlap2_1)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      gene_interest <- c_genes_uploaded()
      df1_c1 <- lapply(gene_interest, function(x) subset(c1, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(gene_interest, function(x) subset(c2, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c3 <- lapply(gene_interest, function(x) subset(c3, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      total3 <- as.data.frame(data.table::rbindlist(df1_c3))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      df1_c3 <- c(df1_c3, list(total = total3))
      
      list_num <- input$c_goi_num_inputs
      f1_goi <-df1_c1[[list_num]]
      f2_goi <-df1_c2[[list_num]]
      f3_goi <-df1_c3[[list_num]]
      overlap2_1 <- subset(f2_goi, gene %in% f1_goi$gene)
      overlap2_3 <- subset(f2_goi, gene %in% f3_goi$gene)
      overlap2_1_3 <- subset(f2_goi, gene %in% f1_goi$gene & gene %in% f3_goi$gene)
      list(f2_goi=f2_goi, overlap2_1=overlap2_1, overlap2_3=overlap2_3, overlap2_1_3=overlap2_1_3)
    }
  })
  
  #comparison for file3 goi
  c_goi_compare3 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      gene_interest <- c_genes_uploaded()
      df1_c1 <- lapply(gene_interest, function(x) subset(c1, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(gene_interest, function(x) subset(c2, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c3 <- lapply(gene_interest, function(x) subset(c3, gene %in% x & 
                                                           pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      total3 <- as.data.frame(data.table::rbindlist(df1_c3))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      df1_c3 <- c(df1_c3, list(total = total3))
      
      list_num <- input$c_goi_num_inputs
      f1_goi <-df1_c1[[list_num]]
      f2_goi <-df1_c2[[list_num]]
      f3_goi <-df1_c3[[list_num]]
      overlap3_1 <- subset(f3_goi, gene %in% f1_goi$gene)
      overlap3_2 <- subset(f3_goi, gene %in% f2_goi$gene)
      overlap3_1_2 <- subset(f3_goi, gene %in% f1_goi$gene & gene %in% f2_goi$gene)
      list(f3_goi=f3_goi, overlap3_1=overlap3_1, overlap3_2=overlap3_2, overlap3_1_2=overlap3_1_2)
    }
  })
  
  c_f1_unique_goi <- reactive({
    d <- c_goi_compare1()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1 <- subset(d$f1_goi, gene %!in% (d$overlap1_2)$gene)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1 <- subset(d$f1_goi, gene %!in% (d$overlap1_2)$gene & gene %!in% (d$overlap1_3)$gene & gene %!in% (d$overlap1_2_3)$gene)
    }
    d1 <- data.frame(f1$gene)
    colnames(d1) <- c("f1")    
    d1 <- unique(sort(d1$f1))
  })
  
  c_f2_unique_goi <- reactive({
    d <- c_goi_compare2()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f2 <- subset(d$f2_goi, gene %!in% (d$overlap2_1)$gene)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f2 <- subset(d$f2_goi, gene %!in% (d$overlap2_1)$gene & gene %!in% (d$overlap2_3)$gene & gene %!in% (d$overlap2_1_3)$gene)
    }
    d1 <- data.frame(f2$gene)
    colnames(d1) <- c("f2")    
    d1 <- unique(sort(d1$f2))
  })
  
  c_f1_and_f2_goi <- reactive({
    d <- c_goi_compare2()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f2 <- d$overlap2_1
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f2 <- subset(d$overlap2_1, gene %!in% (d$overlap2_1_3)$gene)
    }
    d1 <- data.frame(f2$gene)
    colnames(d1) <- c("f12")    
    d1 <- unique(sort(d1$f12))
  })
  
  c_f3_unique_goi <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_goi_compare3()
      f3 <- subset(d$f3_goi, gene %!in% (d$overlap3_1)$gene & gene %!in% (d$overlap3_2)$gene & gene %!in% (d$overlap3_1_2)$gene)
      d1 <- data.frame(f3$gene)
      colnames(d1) <- c("f3")    
      d1 <- unique(sort(d1$f3))
    }
  })
  
  c_f1_and_f3_goi <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_goi_compare3()
      f3 <- subset(d$overlap3_1, gene %!in% (d$overlap3_1_2)$gene)
      d1 <- data.frame(f3$gene)
      colnames(d1) <- c("f13")    
      d1 <- unique(sort(d1$f13))
    }
  })
  
  c_f2_and_f3_goi <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_goi_compare3()
      f3 <- subset(d$overlap3_2, gene %!in% (d$overlap3_1_2)$gene)
      d1 <- data.frame(f3$gene)
      colnames(d1) <- c("f23")    
      d1 <- unique(sort(d1$f23))
    }
  })
  
  c_f1_f2_and_f3_goi <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_goi_compare3()
      d1 <- data.frame((d$overlap3_1_2)$gene)
      colnames(d1) <- c("f123")    
      d1 <- unique(sort(d1$f123))
    }
  })
  
  c_unique_goi_dat <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1 <- c_f1_unique_goi()
      f2 <- c_f2_unique_goi()
      f12 <- c_f1_and_f2_goi()
      n <- max(length(f1), length(f2), length(f12))
      length(f1) <- n
      length(f2) <- n
      length(f12) <- n
      d <- cbind(f1, f2, f12)
      d[is.na(d)] <- " "
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1 <- c_f1_unique_goi()
      f2 <- c_f2_unique_goi()
      f12 <- c_f1_and_f2_goi()
      f3 <- c_f3_unique_goi()
      f13 <- c_f1_and_f3_goi()
      f23 <- c_f2_and_f3_goi()
      f123 <- c_f1_f2_and_f3_goi()
      n <- max(length(f1), length(f2), length(f12), length(f3), length(f13), length(f23), length(f123))
      length(f1) <- n
      length(f2) <- n
      length(f12) <- n
      length(f3) <- n
      length(f13) <- n
      length(f23) <- n
      length(f123) <- n
      d <- cbind(f1, f2, f12, f3, f13, f23, f123)
      d[is.na(d)] <- " "
    }
    as.data.frame(d)
  })
  
  c_venndiagram_snp <- reactive({
    validate(
      need(!is.null(c_snp()), ""),
      need(!is.null(c_vp1_snp_layer()), ""),
      need(!is.null(c_vp2_snp_layer()), "")
    )
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      if(!is.null(c_pd1()) & !is.null(c_pd2())){
        c1 <- c_pd1()
        c2 <- c_pd2()
        snp2gene <- c_SNP_to_gene()
        snp_interest <- split(snp2gene$gene, snp2gene$snpid)

        df1_c1 <- lapply(snp_interest, function(x) subset(c1, gene %in% x & 
                                                            pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
        df1_c2 <- lapply(snp_interest, function(x) subset(c2, gene %in% x & 
                                                            pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
        
        total1 <- as.data.frame(data.table::rbindlist(df1_c1))
        total2 <- as.data.frame(data.table::rbindlist(df1_c2))
        
        df1_c1 <- c(df1_c1, list(total = total1))
        df1_c2 <- c(df1_c2, list(total = total2))

        list_num <- input$c_snp_num_inputs

        x <- list()
        x[["File1"]] <-df1_c1[[list_num]]$gene
        x[["File2"]] <-df1_c2[[list_num]]$gene
        
        vd_title <- list_num
        colors2 = c("gray", "gray")
        
        v0 <- venn.diagram(x,
                           fill = colors2, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                           sub = " ", sub.pos = c(0, 0), euler.d = F, scaled = F, main = vd_title,
                           cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05),
                           fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
      }
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      if(!is.null(c_pd1()) & !is.null(c_pd2()) & !is.null(c_pd3())){
        c1 <- c_pd1()
        c2 <- c_pd2()
        c3 <- c_pd3()
        snp2gene <- c_SNP_to_gene()
        snp_interest <- split(snp2gene$gene, snp2gene$snpid)
        df1_c1 <- lapply(snp_interest, function(x) subset(c1, gene %in% x & 
                                                            pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
        df1_c2 <- lapply(snp_interest, function(x) subset(c2, gene %in% x & 
                                                            pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
        df1_c3 <- lapply(snp_interest, function(x) subset(c3, gene %in% x & 
                                                            pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
        
        total1 <- as.data.frame(data.table::rbindlist(df1_c1))
        total2 <- as.data.frame(data.table::rbindlist(df1_c2))
        total3 <- as.data.frame(data.table::rbindlist(df1_c3))
        
        df1_c1 <- c(df1_c1, list(total = total1))
        df1_c2 <- c(df1_c2, list(total = total2))
        df1_c3 <- c(df1_c3, list(total = total3))
        
        
        list_num <- input$c_snp_num_inputs
        
        x <- list()
        x[["File1"]] <-df1_c1[[list_num]]$gene
        x[["File2"]] <-df1_c2[[list_num]]$gene
        x[["File3"]] <-df1_c3[[list_num]]$gene
        
        vd_title <- list_num
        colors3 = c("gray", "gray", "gray")
        
        v0 <- venn.diagram(x, 
                           fill = colors3, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                           sub = " ", sub.pos = c(0, 0, 0), euler.d = F, scaled = F, main = vd_title,
                           # cat.cex = 1.1, cex = 2, cat.pos = c(180,180,0), cat.dist = c(0.05,0.05,0.05),
                           fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
      }
    }
  })
  
  #comparison for file1 snp
  c_snp_compare1 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      snp2gene <- c_SNP_to_gene()
      snp_interest <- split(snp2gene$gene, snp2gene$snpid)
      df1_c1 <- lapply(snp_interest, function(x) subset(c1, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(snp_interest, function(x) subset(c2, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      
      list_num <- input$c_snp_num_inputs
      f1_snp <-df1_c1[[list_num]]
      f2_snp <-df1_c2[[list_num]]
      overlap1_2 <- subset(f1_snp, gene %in% f2_snp$gene)
      list(f1_snp=f1_snp, overlap1_2=overlap1_2)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      snp2gene <- c_SNP_to_gene()
      snp_interest <- split(snp2gene$gene, snp2gene$snpid)
      df1_c1 <- lapply(snp_interest, function(x) subset(c1, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(snp_interest, function(x) subset(c2, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c3 <- lapply(snp_interest, function(x) subset(c3, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      total3 <- as.data.frame(data.table::rbindlist(df1_c3))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      df1_c3 <- c(df1_c3, list(total = total3))
      
      list_num <- input$c_snp_num_inputs
      f1_snp <-df1_c1[[list_num]]
      f2_snp <-df1_c2[[list_num]]
      f3_snp <-df1_c3[[list_num]]
      overlap1_2 <- subset(f1_snp, gene %in% f2_snp$gene)
      overlap1_3 <- subset(f1_snp, gene %in% f3_snp$gene)
      overlap1_2_3 <- subset(f1_snp, gene %in% f2_snp$gene & gene %in% f3_snp$gene)
      list(f1_snp=f1_snp, overlap1_2=overlap1_2, overlap1_3=overlap1_3, overlap1_2_3=overlap1_2_3)
    }
  })
  
  #comparison for file2 snp
  c_snp_compare2 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      snp2gene <- c_SNP_to_gene()
      snp_interest <- split(snp2gene$gene, snp2gene$snpid)
      df1_c1 <- lapply(snp_interest, function(x) subset(c1, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(snp_interest, function(x) subset(c2, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      
      list_num <- input$c_snp_num_inputs
      f1_snp <-df1_c1[[list_num]]
      f2_snp <-df1_c2[[list_num]]
      overlap2_1 <- subset(f2_snp, gene %in% f1_snp$gene)
      list(f2_snp=f2_snp, overlap2_1=overlap2_1)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      snp2gene <- c_SNP_to_gene()
      snp_interest <- split(snp2gene$gene, snp2gene$snpid)
      df1_c1 <- lapply(snp_interest, function(x) subset(c1, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(snp_interest, function(x) subset(c2, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c3 <- lapply(snp_interest, function(x) subset(c3, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) ) 
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      total3 <- as.data.frame(data.table::rbindlist(df1_c3))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      df1_c3 <- c(df1_c3, list(total = total3))
      
      list_num <- input$c_snp_num_inputs
      f1_snp <-df1_c1[[list_num]]
      f2_snp <-df1_c2[[list_num]]
      f3_snp <-df1_c3[[list_num]]
      overlap2_1 <- subset(f2_snp, gene %in% f1_snp$gene)
      overlap2_3 <- subset(f2_snp, gene %in% f3_snp$gene)
      overlap2_1_3 <- subset(f2_snp, gene %in% f1_snp$gene & gene %in% f3_snp$gene)
      list(f2_snp=f2_snp, overlap2_1=overlap2_1, overlap2_3=overlap2_3, overlap2_1_3=overlap2_1_3)
    }
  })
  
  #comparison for file3 snp
  c_snp_compare3 <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      c1 <- c_pd1()
      c2 <- c_pd2()
      c3 <- c_pd3()
      snp2gene <- c_SNP_to_gene()
      snp_interest <- split(snp2gene$gene, snp2gene$snpid)
      df1_c1 <- lapply(snp_interest, function(x) subset(c1, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c2 <- lapply(snp_interest, function(x) subset(c2, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      df1_c3 <- lapply(snp_interest, function(x) subset(c3, gene %in% x & 
                                                          pvalue < input$c_pval_thresh & FDR < input$c_fdr_thresh & logFC > input$c_logfc_thresh_comb) )  
      
      total1 <- as.data.frame(data.table::rbindlist(df1_c1))
      total2 <- as.data.frame(data.table::rbindlist(df1_c2))
      total3 <- as.data.frame(data.table::rbindlist(df1_c3))
      
      df1_c1 <- c(df1_c1, list(total = total1))
      df1_c2 <- c(df1_c2, list(total = total2))
      df1_c3 <- c(df1_c3, list(total = total3))
      
      list_num <- input$c_snp_num_inputs
      f1_snp <-df1_c1[[list_num]]
      f2_snp <-df1_c2[[list_num]]
      f3_snp <-df1_c3[[list_num]]
      overlap3_1 <- subset(f3_snp, gene %in% f1_snp$gene)
      overlap3_2 <- subset(f3_snp, gene %in% f2_snp$gene)
      overlap3_1_2 <- subset(f3_snp, gene %in% f1_snp$gene & gene %in% f2_snp$gene)
      list(f3_snp=f3_snp, overlap3_1=overlap3_1, overlap3_2=overlap3_2, overlap3_1_2=overlap3_1_2)
    }
  })
  
  c_f1_unique_snp <- reactive({
    d <- c_snp_compare1()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1 <- subset(d$f1_snp, gene %!in% (d$overlap1_2)$gene)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1 <- subset(d$f1_snp, gene %!in% (d$overlap1_2)$gene & gene %!in% (d$overlap1_3)$gene & gene %!in% (d$overlap1_2_3)$gene)
    }
    d1 <- data.frame(f1$gene)
    colnames(d1) <- c("f1")    
    d1 <- unique(sort(d1$f1))
  })
  
  c_f2_unique_snp <- reactive({
    d <- c_snp_compare2()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f2 <- subset(d$f2_snp, gene %!in% (d$overlap2_1)$gene)
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f2 <- subset(d$f2_snp, gene %!in% (d$overlap2_1)$gene & gene %!in% (d$overlap2_3)$gene & gene %!in% (d$overlap2_1_3)$gene)
    }
    d1 <- data.frame(f2$gene)
    colnames(d1) <- c("f2")    
    d1 <- unique(sort(d1$f2))
  })
  
  c_f1_and_f2_snp <- reactive({
    d <- c_snp_compare2()
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f2 <- d$overlap2_1
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f2 <- subset(d$overlap2_1, gene %!in% (d$overlap2_1_3)$gene)
    }
    d1 <- data.frame(f2$gene)
    colnames(d1) <- c("f12")    
    d1 <- unique(sort(d1$f12))
  })
  
  c_f3_unique_snp <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_snp_compare3()
      f3 <- subset(d$f3_snp, gene %!in% (d$overlap3_1)$gene & gene %!in% (d$overlap3_2)$gene & gene %!in% (d$overlap3_1_2)$gene)
      d1 <- data.frame(f3$gene)
      colnames(d1) <- c("f3")    
      d1 <- unique(sort(d1$f3))
    }
  })
  
  c_f1_and_f3_snp <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_snp_compare3()
      f3 <- subset(d$overlap3_1, gene %!in% (d$overlap3_1_2)$gene)
      d1 <- data.frame(f3$gene)
      colnames(d1) <- c("f13")    
      d1 <- unique(sort(d1$f13))
    }
  })
  
  c_f2_and_f3_snp <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_snp_compare3()
      f3 <- subset(d$overlap3_2, gene %!in% (d$overlap3_1_2)$gene)
      d1 <- data.frame(f3$gene)
      colnames(d1) <- c("f23")    
      d1 <- unique(sort(d1$f23))
    }
  })
  
  c_f1_f2_and_f3_snp <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      d <- c_snp_compare3()
      d1 <- data.frame((d$overlap3_1_2)$gene)
      colnames(d1) <- c("f123")    
      d1 <- unique(sort(d1$f123))
    }
  })
  
  c_unique_snp_dat <- reactive({
    if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
      f1 <- c_f1_unique_snp()
      f2 <- c_f2_unique_snp()
      f12 <- c_f1_and_f2_snp()
      n <- max(length(f1), length(f2), length(f12))
      length(f1) <- n
      length(f2) <- n
      length(f12) <- n
      d <- cbind(f1, f2, f12)
      d[is.na(d)] <- " "
    } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
      f1 <- c_f1_unique_snp()
      f2 <- c_f2_unique_snp()
      f12 <- c_f1_and_f2_snp()
      f3 <- c_f3_unique_snp()
      f13 <- c_f1_and_f3_snp()
      f23 <- c_f2_and_f3_snp()
      f123 <- c_f1_f2_and_f3_snp()
      n <- max(length(f1), length(f2), length(f12), length(f3), length(f13), length(f23), length(f123))
      length(f1) <- n
      length(f2) <- n
      length(f12) <- n
      length(f3) <- n
      length(f13) <- n
      length(f23) <- n
      length(f123) <- n
      d <- cbind(f1, f2, f12, f3, f13, f23, f123)
      d[is.na(d)] <- " "
    }
    as.data.frame(d)
  })
  
  c_pf_db <- reactive({
    if(!is.null(input$c_pfam_db)){
      pf_db <- input$c_pfam_db
    }
  })
  
  c_pf_db_search <- reactive({
    pf_db <- c_pf_db()
    selected_pf <- prot_fam[grep(paste(pf_db,collapse='|'), names(prot_fam))]
    selected_pf <- lapply(selected_pf, function(x) x[!is.na(x)])
    selected_pf
  })
  
  c_pf_db_vp1 <- reactive({
    validate(
      need(input$c_file_pulldown1 != '', "")
    )
    if(!is.null(c_pf_db_search())){
      d <- c_pd1()
      gene_interest <- c_pf_db_search()
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )
      list(d_g2s=d_g2s)
    }
  })
  
  c_pf_db_vp2 <- reactive({
    validate(
      need(input$c_file_pulldown2 != '', "")
    )
    if(!is.null(c_pf_db_search())){
      d <- c_pd2()
      gene_interest <- c_pf_db_search()
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )
      list(d_g2s=d_g2s)
    }
  })
  
  c_pf_db_vp3 <- reactive({
    validate(
      need(input$c_file_pulldown3 != '', "")
    )
    if(!is.null(c_pf_db_search())){
      d <- c_pd3()
      gene_interest <- c_pf_db_search()
      d_g2s <- lapply(gene_interest, function(x) subset(d, gene %in% x) )
      list(d_g2s=d_g2s)
    }
  })
  
  c_pf1_cleanup <- reactive({
    validate(
      need(input$c_file_pulldown1 != '', ""),
      need(input$c_pfam_db != '', "")
    )
    d <- c_orig_pd1()
    d_col <- colnames(d)
    if("rep1" %in% d_col & "rep2" %in% d_col){
      prot_remove <- unique(unlist(c_pf_db_search()))
      prot_cleaned <- subset(d, gene %!in% prot_remove)
      prot_cleaned
    }
  })
  
  c_pf2_cleanup <- reactive({
    validate(
      need(input$c_file_pulldown2 != '', ""),
      need(input$c_pfam_db != '', "")
    )
    d <- c_orig_pd2()
    d_col <- colnames(d)
    if("rep1" %in% d_col & "rep2" %in% d_col){
      prot_remove <- unique(unlist(c_pf_db_search()))
      prot_cleaned <- subset(d, gene %!in% prot_remove)
      prot_cleaned
    }
  })
  
  c_pf3_cleanup <- reactive({
    validate(
      need(input$c_file_pulldown3 != '', ""),
      need(input$c_pfam_db != '', "")
    )
    d <- c_orig_pd3()
    d_col <- colnames(d)
    if("rep1" %in% d_col & "rep2" %in% d_col){
      prot_remove <- unique(unlist(c_pf_db_search()))
      prot_cleaned <- subset(d, gene %!in% prot_remove)
      prot_cleaned
    }
  })
  
  c_vp1_pf_db_layer <- reactive({
    validate(
      need(!is.null(c_pd1()), ""),
      need(!is.null(c_pf_db()), "")
    )
    d <- c_pd1()
    c1_pf_db <- c_pf_db_vp1()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Purples", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "Purples", showlegend = T, width = 300, height = 390)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      # p <- plot_volcano_exac_multi(below_thresh, above_thresh, no_exist)
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             legend = list(orientation = 'h', y = -0.23))
    if(input$c_colorscheme == "fdr" | input$c_colorscheme == "exac"){
        df <- ldply(c1_pf_db$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$c_marker_text_prot_fam_db == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes(p, df)
          } else if(input$c_marker_text_prot_fam_db == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none(p, d)
          p <- vp_layer_no_genes
        }
      
    } else if(input$c_colorscheme == "cbf"){
        df <- ldply(c1_pf_db$d_g2s, data.frame)
        if(nrow(df) != 0){
          if(input$c_marker_text_prot_fam_db == "yes_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf(p, df)
          } else if(input$c_marker_text_prot_fam_db == "no_label"){
            vp_layer_genes <- vp_layer_for_uploaded_genes_cbf_no_text(p, df)
          }
          p <- vp_layer_genes
        } else{
          vp_layer_no_genes <- vp_layer_for_uploaded_genes_none_cbf(p, d)
          p <- vp_layer_no_genes
        }
      
    }
    p <- p %>% 
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp2_pf_db_layer <- reactive({
    validate(
      need(!is.null(c_pd2()), ""),
      need(!is.null(c_pf_db()), "")
    )
    d <- c_pd2()
    c2_pf_db <- c_pf_db_vp2()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Purples", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "Purples", showlegend = T, width = 300, height = 390)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             legend = list(orientation = 'h', y = -0.23))
    if(input$c_colorscheme == "fdr" | input$c_colorscheme == "exac"){
      df <- ldply(c2_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes_no_text(p, df)
        }
        p <- vp_layer_genes
      } else{
        vp_layer_no_genes <- vp_layer_for_uploaded_genes_none(p, d)
        p <- vp_layer_no_genes
      }
      
    } else if(input$c_colorscheme == "cbf"){
      df <- ldply(c2_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes_cbf(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes_cbf_no_text(p, df)
        }
        p <- vp_layer_genes
      } else{
        vp_layer_no_genes <- vp_layer_for_uploaded_genes_none_cbf(p, d)
        p <- vp_layer_no_genes
      }
      
    }
    p <- p %>% 
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_vp3_pf_db_layer <- reactive({
    validate(
      need(!is.null(c_pd3()), ""),
      need(!is.null(c_pf_db()), "")
    )
    d <- c_pd3()
    c3_pf_db <- c_pf_db_vp3()
    min_x <- c_min_x()
    min_y <- c_min_y()
    max_x <- c_max_x()
    max_y <- c_max_y()
    if(input$c_colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Purples", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    } else if(input$c_colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "Purples", showlegend = T, width = 300, height = 390)
      p <- add_markers(p, data = below_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.8, 
                       text = ~paste(gene), hoverinfo = "text")
      p
    } else if(input$c_colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$c_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = T, width = 300, height = 390)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~logFC, y = ~-log10(pvalue),
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
      p 
    }
    p <- p%>%
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F),
             legend = list(orientation = 'h', y = -0.23))
    if(input$c_colorscheme == "fdr" | input$c_colorscheme == "exac"){
      df <- ldply(c3_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes_no_text(p, df)
        }
        p <- vp_layer_genes
      } else{
        vp_layer_no_genes <- vp_layer_for_uploaded_genes_none(p, d)
        p <- vp_layer_no_genes
      }
      
    } else if(input$c_colorscheme == "cbf"){
      df <- ldply(c3_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes_cbf(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          vp_layer_genes <- vp_layer_for_uploaded_genes_cbf_no_text(p, df)
        }
        p <- vp_layer_genes
      } else{
        vp_layer_no_genes <- vp_layer_for_uploaded_genes_none_cbf(p, d)
        p <- vp_layer_no_genes
      }
      
    }
    p <- p %>% 
      layout(xaxis = list(range=c(min_x-0.5, max_x+0.5), showgrid = F), yaxis = list(range=c(min_y-0.5, max_y+0.5), showgrid = F)) %>%
      add_lines(x = c(min_x-0.5, max_x+0.5), y = -log10(input$c_pval_thresh), line = list(dash = "dash", width = 0.5, color = "#2b333e"), 
                name = '', hoverinfo = "text", text = paste0("pvalue = ", input$c_pval_thresh), showlegend = F) %>%
      add_lines(x = input$c_logfc_thresh_comb, y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", input$a_logFC_thresh), showlegend = F) %>%
      add_lines(x = -(input$c_logfc_thresh_comb), y = c(min_y-0.5, max_y+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"), 
                name = '', hoverinfo = "text", text = paste0("logFC = ", -(input$a_logFC_thresh)), showlegend = F)
  })
  
  c_sp1_pf_db_layer <- reactive({
    validate(
      need(!is.null(c_pd1()), ""),
      need(!is.null(c_pf_db()), "")
    )
    d <- c_pd1()
    c1_pf_db <- c_pf_db_vp1()
    cc <- c_sp1_cor()
    if(input$colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Purples", showlegend = F, width = 320, height = 320) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"),
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    } else if(input$colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "Purples", showlegend = F, width = 320, height = 320) 
      p <- add_lines(p, data = d, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      p <- add_markers(p, data = below_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p
    } else if(input$colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = F, width = 320, height = 320) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    }
    if(input$c_colorscheme == "fdr" | input$c_colorscheme == "exac"){
      df <- ldply(c1_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_no_text(p, df)
        }
        p <- sp_layer_genes
      } else{
        sp_layer_no_genes <- sp_layer_for_uploaded_genes_none(p, d)
        p <- sp_layer_no_genes
      }
    } else if(input$c_colorscheme == "cbf"){
      df <- ldply(c1_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_cbf(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_cbf_no_text(p, df)
        }
        p <- sp_layer_genes
      } else{
        sp_layer_no_genes <- sp_layer_for_uploaded_genes_none_cbf(p, d)
        p <- sp_layer_no_genes
      }
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(d$rep1, d$rep2))-1, (max(d$rep1, d$rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(d$rep1, d$rep2))-1, (max(d$rep1, d$rep2))+1)), 
             title = cc, titlefont = list(size=12))
  })
  
  c_sp2_pf_db_layer <- reactive({
    validate(
      need(!is.null(c_pd2()), ""),
      need(!is.null(c_pf_db()), "")
    )
    d <- c_pd2()
    c2_pf_db <- c_pf_db_vp2()
    cc <- c_sp2_cor()
    if(input$colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Purples", showlegend = F, width = 320, height = 320) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"),
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    } else if(input$colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "Purples", showlegend = F, width = 320, height = 320) 
      p <- add_lines(p, data = d, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      p <- add_markers(p, data = below_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p
    } else if(input$colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = F, width = 320, height = 320) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    }
    if(input$c_colorscheme == "fdr" | input$c_colorscheme == "exac"){
      df <- ldply(c2_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_no_text(p, df)
        }
        p <- sp_layer_genes
      } else{
        sp_layer_no_genes <- sp_layer_for_uploaded_genes_none(p, d)
        p <- sp_layer_no_genes
      }
    } else if(input$c_colorscheme == "cbf"){
      df <- ldply(c2_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_cbf(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_cbf_no_text(p, df)
        }
        p <- sp_layer_genes
      } else{
        sp_layer_no_genes <- sp_layer_for_uploaded_genes_none_cbf(p, d)
        p <- sp_layer_no_genes
      }
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(d$rep1, d$rep2))-1, (max(d$rep1, d$rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(d$rep1, d$rep2))-1, (max(d$rep1, d$rep2))+1)), 
             title = cc, titlefont = list(size=12))
  })
  
  c_sp3_pf_db_layer <- reactive({
    validate(
      need(!is.null(c_pd3()), ""),
      need(!is.null(c_pf_db()), "")
    )
    d <- c_pd3()
    c3_pf_db <- c_pf_db_vp3()
    cc <- c_sp3_cor()
    if(input$colorscheme == "fdr"){
      data <- separate_to_groups_for_color_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Purples", showlegend = F, width = 320, height = 320) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"),
                         opacity = 0.8, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    } else if(input$colorscheme == "exac"){
      d$s <- exac$em_p_hi[match(d$gene, exac$GENE_NAME)]
      d$s[is.na(d$s)] <- 2
      below_thresh <- subset(d, s < 0.9)
      above_thresh <- subset(d, s >= 0.9)
      no_exist <- subset(d, s == 2)
      p <- plot_ly(colors = "Purples", showlegend = F, width = 320, height = 320) 
      p <- add_lines(p, data = d, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      p <- add_markers(p, data = below_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = above_thresh, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p <- add_markers(p, data = no_exist, x = ~rep1, y = ~rep2, 
                       marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                       opacity = 0.7, 
                       text = ~paste(gene), hoverinfo = "text")
      p
    } else if(input$colorscheme == "cbf"){
      data <- separate_to_groups_for_cbf_integrated(d, input$a_fdr_thresh)
      p <- plot_ly(colors = "Greys", showlegend = F, width = 320, height = 320) 
      p <- add_lines(p, data = d, x = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))), y = ~c(ceiling(min(rep1, rep2)), floor(max(rep1, rep2))),
                     line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
      for(i in nrow(data)){
        p <- add_markers(p, data = data, x = ~rep1, y = ~rep2, 
                         marker = list(size = 6, cmin = 0, cmax = 1, color = ~col), #line = list(width=0.1, color = "black"), 
                         opacity = 0.6, 
                         text = ~paste(gene), hoverinfo = "text", name = "pull down")
      }
    }
    if(input$c_colorscheme == "fdr" | input$c_colorscheme == "exac"){
      df <- ldply(c3_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_no_text(p, df)
        }
        p <- sp_layer_genes
      } else{
        sp_layer_no_genes <- sp_layer_for_uploaded_genes_none(p, d)
        p <- sp_layer_no_genes
      }
    } else if(input$c_colorscheme == "cbf"){
      df <- ldply(c3_pf_db$d_g2s, data.frame)
      if(nrow(df) != 0){
        if(input$c_marker_text_prot_fam_db == "yes_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_cbf(p, df)
        } else if(input$c_marker_text_prot_fam_db == "no_label"){
          sp_layer_genes <- sp_layer_for_uploaded_genes_cbf_no_text(p, df)
        }
        p <- sp_layer_genes
      } else{
        sp_layer_no_genes <- sp_layer_for_uploaded_genes_none_cbf(p, d)
        p <- sp_layer_no_genes
      }
    }
    p <- p %>%
      layout(xaxis = list(title = "rep1", range=~c((min(d$rep1, d$rep2))-1, (max(d$rep1, d$rep2))+1)), 
             yaxis = list(title = "rep2", range=~c((min(d$rep1, d$rep2))-1, (max(d$rep1, d$rep2))+1)), 
             title = cc, titlefont = list(size=12))
  })
  
  output$c_FDR_colorbar <- renderPlot({
    validate(
      need(input$c_file_pulldown1 != '', "")
    )
    c_vp_colorbar_dl()
  })
  
  output$c_inweb_colorbar <- renderPlot({
    validate(
      need(!is.null(c_vp2_inweb()), "")
    )
    c_vp_colorbar_dl()
  })
  
  output$c_goi_colorbar <- renderPlot({
    validate(
      need(!is.null(c_goi_vp2()), "")
    )
    c_vp_colorbar_dl()
  })
  
  output$c_snp_colorbar <- renderPlot({
    validate(
      need(!is.null(c_snp_vp2()), "")
    )
    c_vp_colorbar_dl()
  })
  
  output$VolcanoPlot_c1 <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file")
    )
    if(is.null(c_pf_db())){
      if(is.null(c_search_gene())){
        c_vp1()
      } else{
        c_vp1_plus()
      }
    } else if(!is.null(c_pf_db())){
      c_vp1_pf_db_layer()
    }
  })
  
  
  output$VolcanoPlot_c1_inweb <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file")
    )
    if(is.null(c_search_gene())){
      c_vp1_inweb_layer()
    } else{
      c_vp1_inweb_plus()
    }
  })
  
  output$VolcanoPlot_c1_goi <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file")
    )
    if(is.null(c_search_gene())){
      c_vp1_goi_layer()
    } else{
      c_vp1_goi_plus()
    }
  })
  
  output$VolcanoPlot_c1_snp <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file")
    )
    if(is.null(c_search_gene())){
      c_vp1_snp_layer()
    } else{
      c_vp1_snp_plus()
    }
  })
  
  output$VolcanoPlot_c2 <- renderPlotly({
    validate(
      need(input$c_file_pulldown2 != '', "Upload file")
    )
    if(is.null(c_pf_db())){
      if(is.null(c_search_gene())){
        c_vp2()
      } else{
        c_vp2_plus()
      }
    } else if(!is.null(c_pf_db())){
      c_vp2_pf_db_layer()
    }
  })
  
  output$VolcanoPlot_c2_inweb <- renderPlotly({
    validate(
      need(input$c_file_pulldown2 != '', "Upload file")
    )
    if(is.null(c_search_gene())){
      c_vp2_inweb_layer()
    } else{
      c_vp2_inweb_plus()
    }
  })
  
  output$VolcanoPlot_c2_goi <- renderPlotly({
    validate(
      need(input$c_file_pulldown2 != '', "Upload file")
    )
    if(is.null(c_search_gene())){
      c_vp2_goi_layer()
    } else{
      c_vp2_goi_plus()
    }
  })
  
  output$VolcanoPlot_c2_snp <- renderPlotly({
    validate(
      need(input$c_file_pulldown2 != '', "Upload file")
    )
    if(is.null(c_search_gene())){
      c_vp2_snp_layer()
    } else{
      c_vp2_snp_plus()
    }
  })
  
  output$VolcanoPlot_c3 <- renderPlotly({
    validate(
      need(input$c_file_pulldown3 != '', "Upload file")
    )
    if(is.null(c_pf_db())){
      if(is.null(c_search_gene())){
        c_vp3()
      } else{
        c_vp3_plus()
      }
    } else if(!is.null(c_pf_db())){
      c_vp3_pf_db_layer()
    }
  })
  
  output$VolcanoPlot_c3_inweb <- renderPlotly({
    validate(
      need(input$c_file_pulldown3 != '', "Upload file")
    )
    if(is.null(c_search_gene())){
      c_vp3_inweb_layer()
    } else{
      c_vp3_inweb_plus()
    }
  })
  
  output$VolcanoPlot_c3_goi <- renderPlotly({
    validate(
      need(input$c_file_pulldown3 != '', "Upload file")
    )
    if(is.null(c_search_gene())){
      c_vp3_goi_layer()
    } else{
      c_vp3_goi_plus()
    }
  })
  
  output$VolcanoPlot_c3_snp <- renderPlotly({
    validate(
      need(input$c_file_pulldown3 != '', "Upload file")
    )
    if(is.null(c_search_gene())){
      c_vp3_snp_layer()
    } else{
      c_vp3_snp_plus()
    }
  })
  
  output$c_VennDiagram_inweb <- renderPlot({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', "")
    )
    v0 <- c_venndiagram_inweb()
    grid.newpage()
    grid.draw(v0)
  })
  
  output$c_VennDiagram_goi <- renderPlot({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', "")
    )
    v0 <- c_venndiagram_goi()
    grid.newpage()
    grid.draw(v0)
  })
  
  output$c_VennDiagram_snp <- renderPlot({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', "")
    )
    v0 <- c_venndiagram_snp()
    grid.newpage()
    grid.draw(v0)
  })
  
  output$ScatterPlot_c1 <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file")
    )
    input_file <- c_in_pd1()
    d_col <- colnames(input_file)
    if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
       "rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(c_pf_db())){
        if(is.null(c_search_gene())){
          c_sp1()
        } else{
          c_sp1_plus()
        }
      }else if(!is.null(c_pf_db())){
        c_sp1_pf_db_layer()
      }
    } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
      validate(
        need("rep1" %in% d_col & "rep2" %in% d_col, "Must have rep1 and rep2 values.")
      )
    } else if("rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(c_pf_db())){
        if(is.null(c_search_gene())){
          c_sp1()
        } else{
          c_sp1_plus()
        }
      }else if(!is.null(c_pf_db())){
        c_sp1_pf_db_layer()
      }
    }
  })
  
  output$ScatterPlot_c2 <- renderPlotly({
    validate(
      need(input$c_file_pulldown2 != '', "Upload file")
    )
    input_file <- c_in_pd2()
    d_col <- colnames(input_file)
    if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
       "rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(c_pf_db())){
        if(is.null(c_search_gene())){
          c_sp2()
        } else{
          c_sp2_plus()
        }
      }else if(!is.null(c_pf_db())){
        c_sp2_pf_db_layer()
      }
    } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
      validate(
        need("rep1" %in% d_col & "rep2" %in% d_col, "Must have rep1 and rep2 values.")
      )
    } else if("rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(c_pf_db())){
        if(is.null(c_search_gene())){
          c_sp2()
        } else{
          c_sp2_plus()
        }
      }else if(!is.null(c_pf_db())){
        c_sp2_pf_db_layer()
      }
    }
  })
  
  output$ScatterPlot_c3 <- renderPlotly({
    validate(
      need(input$c_file_pulldown3 != '', "Upload file")
    )
    input_file <- c_in_pd3()
    d_col <- colnames(input_file)
    if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
       "rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(c_pf_db())){
        if(is.null(c_search_gene())){
          c_sp3()
        } else{
          c_sp3_plus()
        }
      }else if(!is.null(c_pf_db())){
        c_sp3_pf_db_layer()
      }
    } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col){
      validate(
        need("rep1" %in% d_col & "rep2" %in% d_col, "Must have rep1 and rep2 values.")
      )
    } else if("rep1" %in% d_col & "rep2" %in% d_col){
      if(is.null(c_pf_db())){
        if(is.null(c_search_gene())){
          c_sp3()
        } else{
          c_sp3_plus()
        }
      }else if(!is.null(c_pf_db())){
        c_sp3_pf_db_layer()
      }
    }
  })
  
  output$c_comparison_text <- renderUI({
    validate(
      need(input$c_file_pulldown1 != '', ""),
      need(input$c_file_pulldown2 != '', "")
    )
    HTML("<b>User-defined significance thresholds:</b>")
  })
  
  output$comparison1 <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', "")
    )
    if(is.null(c_search_gene())){
      c_compare1_plot()
    } else{
      c_compare1_plus()
    }
  })
  
  output$comparison1_pf <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', "")
    )
    c_compare1_pfe_plot()
  })
  
  output$comparison2 <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', "")
    )
    if(is.null(c_search_gene())){
      c_compare2_plot()
    } else{
      c_compare2_plus()
    }
  })
  
  output$comparison2_pf <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', "")
    )
    c_compare2_pfe_plot()
  })
  
  output$comparison3 <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', ""),
      need(input$c_file_pulldown3 != '', "")
    )
    if(is.null(c_search_gene())){
      c_compare3_plot()
    } else{
      c_compare3_plus()
    }
  })
  
  output$comparison3_pf <- renderPlotly({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', ""),
      need(input$c_file_pulldown3 != '', "")
    )
    c_compare3_pfe_plot()
  })
  
  output$c_VennDiagram <- renderPlot({
    validate(
      need(input$c_file_pulldown1 != '', "Upload file"),
      need(input$c_file_pulldown2 != '', "")
    )
    v0 <- c_venndiagram()
    grid.newpage()
    grid.draw(v0)
  })
  
  output$c_unique <- renderTable({
    validate(
      need(input$c_file_pulldown1 != '', ""),
      need(input$c_file_pulldown2 != '', "")
    )
    c_unique_dat()
  },
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$c_unique_inweb <- renderTable({
    validate(
      need(input$c_file_pulldown1 != '', ""),
      need(input$c_file_pulldown2 != '', "")
    )
    c_unique_inweb_dat()
  },
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$c_unique_goi <- renderTable({
    validate(
      need(!is.null(c_vp1_goi_layer()), ""),
      need(!is.null(c_vp2_goi_layer()), "")
    )
    c_unique_goi_dat()
  },
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$c_unique_snp <- renderTable({
    validate(
      need(!is.null(c_vp1_snp_layer()), ""),
      need(!is.null(c_vp2_snp_layer()), "")
    )
    c_unique_snp_dat()
  },
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

  output$c1_download_mapped_uniprot <- downloadHandler(
    filename = function() {
      paste("protein-to-gene-names1", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_pd1_converted(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$c1_download_replications_calculated <- downloadHandler(
    filename = function() {
      paste("input_calculated1", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_pd1(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$c2_download_mapped_uniprot <- downloadHandler(
    filename = function() {
      paste("protein-to-gene-names2", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_pd2_converted(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$c2_download_replications_calculated <- downloadHandler(
    filename = function() {
      paste("input_calculated2", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_pd2(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$c3_download_mapped_uniprot <- downloadHandler(
    filename = function() {
      paste("protein-to-gene-names3", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_pd3_converted(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$c3_download_replications_calculated <- downloadHandler(
    filename = function() {
      paste("input_calculated3", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_pd3(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$c_download_snp_to_genes <- downloadHandler(
    filename = function() {
      paste("snp-to-gene_mfc", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_SNP_to_gene(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$c_download_basic_plots <- downloadHandler(
    filename = "quality-control-mfc.html",
    content = function(file) {
      if(!is.null(input$c_file_pulldown1) & is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
        df <- c_orig_pd1()
        
        d_all5 <- c("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df) &
                      "rep1" %in% colnames(df) & "rep2" %in% colnames(df))
        d_ttest <- c("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df))
        d_reps <- c("rep1" %in% colnames(df) & "rep2" %in% colnames(df))
        
        if(d_all5){
          if(is.null(c_search_gene())){
            params <- list(a = c_vp1(), b = c_sp1(), g = c_vp_colorbar_dl())
          } else{
            params <- list(a = c_vp1_plus(), b = c_sp1_plus(), g = c_vp_colorbar_dl())
          }
        } else if(d_ttest){
          if(is.null(c_search_gene())){
            params <- list(a = c_vp1(), b = c_sp_blank(), g = c_vp_colorbar_dl())
          } else{
            params <- list(a = c_vp1_plus(), g = c_vp_colorbar_dl())
          }
        } else if(d_reps){
          if(is.null(c_search_gene())){
            params <- list(a = c_vp1(), b = c_sp1(), g = c_vp_colorbar_dl())
          } else{
            params <- list(a = c_vp1_plus(), b = c_sp1_plus(), g = c_vp_colorbar_dl())
          }
        }
      } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
        df <- c_orig_pd1()
        df1 <- c_orig_pd2()
        
        d_all5 <- c("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df) &
                      "rep1" %in% colnames(df) & "rep2" %in% colnames(df))
        d_ttest <- c("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df))
        d_reps <- c("rep1" %in% colnames(df) & "rep2" %in% colnames(df))
        
        d1_all5 <- c("logFC" %in% colnames(df1) & "FDR" %in% colnames(df1) & "pvalue" %in% colnames(df1) &
                       "rep1" %in% colnames(df1) & "rep2" %in% colnames(df1))
        d1_ttest <- c("logFC" %in% colnames(df1) & "FDR" %in% colnames(df1) & "pvalue" %in% colnames(df1))
        d1_reps <- c("rep1" %in% colnames(df1) & "rep2" %in% colnames(df1))
        
        if((d_all5 | d_reps) & (d1_all5 | d1_reps)){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), shareY = T) %>% layout(width = 640)
            p1 <- subplot(c_sp1(), c_sp2(), shareY = T) %>% layout(width = 640, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), shareY = T) %>% layout(width = 640)
            p1_plus <- subplot(c_sp1_plus(), c_sp1_plus(), shareY = T) %>% layout(width = 640, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if((d_all5 | d_reps) & d1_ttest){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), shareY = T) %>% layout(width = 640)
            p1 <- subplot(c_sp1(), plot_scatter_white(c_pd1()), shareY = T) %>% layout(width = 640, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), shareY = T) %>% layout(width = 640)
            p1_plus <- subplot(c_sp1_plus(), plot_scatter_white(c_pd1()), shareY = T) %>% layout(width = 640, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if(d_ttest & (d1_all5 | d1_reps)){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), shareY = T) %>% layout(width = 640)
            p1 <- subplot(plot_scatter_white(c_pd2()), c_sp2(), shareY = T) %>% layout(width = 640, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), shareY = T) %>% layout(width = 640)
            p1_plus <- subplot(plot_scatter_white(c_pd2()), c_sp2_plus(), shareY = T) %>% layout(width = 640, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if(d_ttest & d1_ttest){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), shareY = T) %>% layout(width = 640)
            params <- list(a = p, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), shareY = T) %>% layout(width = 640)
            params <- list(a = p_plus, g = c_vp_colorbar_dl())
          }
        } 
      } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
        df <- c_orig_pd1()
        df1 <- c_orig_pd2()
        df2 <- c_orig_pd3()
        
        d_all5 <- c("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df) &
                      "rep1" %in% colnames(df) & "rep2" %in% colnames(df))
        d_ttest <- c("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df))
        d_reps <- c("rep1" %in% colnames(df) & "rep2" %in% colnames(df))
        
        d1_all5 <- c("logFC" %in% colnames(df1) & "FDR" %in% colnames(df1) & "pvalue" %in% colnames(df1) &
                       "rep1" %in% colnames(df1) & "rep2" %in% colnames(df1))
        d1_ttest <- c("logFC" %in% colnames(df1) & "FDR" %in% colnames(df1) & "pvalue" %in% colnames(df1))
        d1_reps <- c("rep1" %in% colnames(df1) & "rep2" %in% colnames(df1))
        
        d2_all5 <- c("logFC" %in% colnames(df2) & "FDR" %in% colnames(df2) & "pvalue" %in% colnames(df2) &
                       "rep1" %in% colnames(df2) & "rep2" %in% colnames(df2))
        d2_ttest <- c("logFC" %in% colnames(df2) & "FDR" %in% colnames(df2) & "pvalue" %in% colnames(df2))
        d2_reps <- c("rep1" %in% colnames(df2) & "rep2" %in% colnames(df2))
        
        if((d_all5 | d_reps) & (d1_all5 | d1_reps) & (d2_all5 | d2_reps)){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), c_vp3(), shareY = T) %>% layout(width = 960)
            p1 <- subplot(c_sp1(), c_sp2(), c_sp3(), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), c_vp3_plus(), shareY = T) %>% layout(width = 960)
            p1_plus <- subplot(c_sp1_plus(), c_sp2_plus(), c_sp3_plus(), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if((d_all5 | d_reps) & (d1_all5 | d1_reps) & d2_ttest){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), c_vp3(), shareY = T) %>% layout(width = 960)
            p1 <- subplot(c_sp1(), c_sp2(), plot_scatter_white(c_pd2()), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), c_vp3_plus(), shareY = T) %>% layout(width = 960)
            p1_plus <- subplot(c_sp1_plus(), c_sp2_plus(), plot_scatter_white(c_pd2()), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if((d_all5 | d_reps) & d1_ttest & (d2_all5 | d2_reps)){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), c_vp3(), shareY = T) %>% layout(width = 960)
            p1 <- subplot(c_sp1(), plot_scatter_white(c_pd1()), c_sp3(), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), c_vp3_plus(), shareY = T) %>% layout(width = 960)
            p1_plus <- subplot(c_sp1_plus(), plot_scatter_white(c_pd1()), c_sp3_plus(), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if((d_all5 | d_reps) & d1_ttest & d2_ttest){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), c_vp3(), shareY = T) %>% layout(width = 960)
            p1 <- subplot(c_sp1(), plot_scatter_white(c_pd1()), plot_scatter_white(c_pd1()), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), c_vp3_plus(), shareY = T) %>% layout(width = 960)
            p1_plus <- subplot(c_sp1_plus(), plot_scatter_white(c_pd1()), plot_scatter_white(c_pd1()), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if(d_ttest & (d1_all5 | d1_reps) & (d2_all5 | d2_reps)){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), c_vp3(), shareY = T) %>% layout(width = 960)
            p1 <- subplot(plot_scatter_white(c_pd2()), c_sp2(), c_sp3(), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), c_vp3_plus(), shareY = T) %>% layout(width = 960)
            p1_plus <- subplot(plot_scatter_white(c_pd2()), c_sp2_plus(), c_sp3_plus(), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if(d_ttest & (d1_all5 | d1_reps) & d2_ttest){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), c_vp3(), shareY = T) %>% layout(width = 960)
            p1 <- subplot(plot_scatter_white(c_pd2()), c_sp2(), plot_scatter_white(c_pd2()), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), c_vp3_plus(), shareY = T) %>% layout(width = 960)
            p1_plus <- subplot(plot_scatter_white(c_pd2()), c_sp2_plus(), plot_scatter_white(c_pd2()), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if(d_ttest & d1_ttest & (d2_all5 | d2_reps)){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), c_vp3(), shareY = T) %>% layout(width = 960)
            p1 <- subplot(plot_scatter_white(c_pd3()), plot_scatter_white(c_pd3()), c_sp3(), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p, b = p1, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), c_vp3_plus(), shareY = T) %>% layout(width = 960)
            p1_plus <- subplot(plot_scatter_white(c_pd3()), plot_scatter_white(c_pd3()), c_sp3_plus(), shareY = T) %>% layout(width = 960, title = "")
            params <- list(a = p_plus, b = p1_plus, g = c_vp_colorbar_dl())
          }
        } else if(d_ttest & d1_ttest & d2_ttest){
          if(is.null(c_search_gene())){
            p <- subplot(c_vp1(), c_vp2(), c_vp3(), shareY = T) %>% layout(width = 960)
            params <- list(a = p, g = c_vp_colorbar_dl())
          } else{
            p_plus <- subplot(c_vp1_plus(), c_vp2_plus(), c_vp3_plus(), shareY = T) %>% layout(width = 960)
            params <- list(a = p_plus, g = c_vp_colorbar_dl())
          }
        }
      }
      rmarkdown::render("scripts/basic-mfc.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$c_download_protein_comparisons <- downloadHandler(
    filename = "comparisons.html",
    content = function(file) {
      if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
        if(is.null(c_search_gene())){
          p <- subplot(c_compare1_plot(), c_compare2_plot(), shareY = T) %>% layout(width = 640)
          params <- list(a = p, d = c_venndiagram(), e = c_unique_dat())
        } else{
          p <- subplot(c_compare1_plus(), c_compare2_plus(), shareY = T) %>% layout(width = 640)
          params <- list(a = p, d = c_venndiagram(), e = c_unique_dat())
        }
      } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
        if(is.null(c_search_gene())){
          p <- subplot(c_compare1_plot(), c_compare2_plot(), c_compare3_plot(), shareY = T) %>% layout(width = 960)
          params <- list(a = p, d = c_venndiagram(), e = c_unique_dat())
        } else{
          p <- subplot(c_compare1_plus(), c_compare2_plus(), c_compare3_plus(), shareY = T) %>% layout(width = 960)
          params <- list(a = p, d = c_venndiagram(), e = c_unique_dat())
        }
      }
      rmarkdown::render("scripts/pc-mfc.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$c_download_inweb <- downloadHandler(
    filename = "MFC-inweb.html",
    content = function(file) {
      if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
        if(is.null(c_search_gene())){
          p <- subplot(c_vp1_inweb_layer(), c_vp2_inweb_layer(), shareY = T) %>% layout(width = 640, title = "")
          params <- list(a = p, d = c_venndiagram_inweb(), e = c_unique_inweb_dat())
        } else{
          p <- subplot(c_vp1_inweb_plus(), c_vp2_inweb_plus(), shareY = T) %>% layout(width = 640, title = "")
          params <- list(a = p, d = c_venndiagram_inweb(), e = c_unique_inweb_dat())
        }
      } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
        if(is.null(c_search_gene())){
          p <- subplot(c_vp1_inweb_layer(), c_vp2_inweb_layer(), c_vp3_inweb_layer(), shareY = T) %>% layout(width = 960, title = "")
          params <- list(a = p, d = c_venndiagram_inweb(), e = c_unique_inweb_dat())
        } else{
          p <- subplot(c_vp1_inweb_plus(), c_vp2_inweb_plus(), c_vp3_inweb_plus(), shareY = T) %>% layout(width = 960, title = "")
          params <- list(a = p, d = c_venndiagram_inweb(), e = c_unique_inweb_dat())
        }
      }
      rmarkdown::render("scripts/pc-mfc.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$c_download_goi <- downloadHandler(
    filename = "MFC-goi.html",
    content = function(file) {
      if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
        if(is.null(c_search_gene())){
          p <- subplot(c_vp1_goi_layer(), c_vp2_goi_layer(), shareY = T) %>% layout(width = 640, title = "")
          params <- list(a = p, d = c_venndiagram_goi(), e = c_unique_goi_dat())
        } else{
          p <- subplot(c_vp1_goi_plus(), c_vp2_goi_plus(), shareY = T) %>% layout(width = 640, title = "")
          params <- list(a = p, d = c_venndiagram_goi(), e = c_unique_goi_dat())
        }
      } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
        if(is.null(c_search_gene())){
          p <- subplot(c_vp1_goi_layer(), c_vp2_goi_layer(), c_vp3_goi_layer(), shareY = T) %>% layout(width = 960, title = "")
          params <- list(a = p, d = c_venndiagram_goi(), e = c_unique_goi_dat())
        } else{
          p <- subplot(c_vp1_goi_plus(), c_vp2_goi_plus(), c_vp3_goi_plus(), shareY = T) %>% layout(width = 960, title = "")
          params <- list(a = p, d = c_venndiagram_goi(), e = c_unique_goi_dat())
        }
      }
      rmarkdown::render("scripts/pc-mfc.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$c_download_snp <- downloadHandler(
    filename = "MFC-snp.html",
    content = function(file) {
      if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
        if(is.null(c_search_gene())){
          p <- subplot(c_vp1_snp_layer(), c_vp2_snp_layer(), shareY = T) %>% layout(width = 640, title = "")
          params <- list(a = p, d = c_venndiagram_snp(), e = c_unique_snp_dat())
        } else{
          p <- subplot(c_vp1_snp_plus(), c_vp2_snp_plus(), shareY = T) %>% layout(width = 640, title = "")
          params <- list(a = p, d = c_venndiagram_snp(), e = c_unique_snp_dat())
        }
      } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
        if(is.null(c_search_gene())){
          p <- subplot(c_vp1_snp_layer(), c_vp2_snp_layer(), c_vp3_snp_layer(), shareY = T) %>% layout(width = 960, title = "")
          params <- list(a = p, d = c_venndiagram_snp(), e = c_unique_snp_dat())
        } else{
          p <- subplot(c_vp1_snp_plus(), c_vp2_snp_plus(), c_vp3_snp_plus(), shareY = T) %>% layout(width = 960, title = "")
          params <- list(a = p, d = c_venndiagram_snp(), e = c_unique_snp_dat())
        }
      }
      rmarkdown::render("scripts/pc-mfc.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$c_download_protein_fams <- downloadHandler(
    filename = "MFC-prot-fam.html",
    content = function(file) {
      if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & is.null(input$c_file_pulldown3)){
        params <- list(a = c_compare1_pfe_plot(), b = c_compare2_pfe_plot())
      } else if(!is.null(input$c_file_pulldown1) & !is.null(input$c_file_pulldown2) & !is.null(input$c_file_pulldown3)){
        params <- list(a = c_compare1_pfe_plot(), b = c_compare2_pfe_plot(), c = c_compare3_pfe_plot())
      }
      rmarkdown::render("scripts/pf-mfc.Rmd", 
                        output_file = file,
                        params = params
      )
    }
  )
  
  output$download_c1_pf_cleaned_input <- downloadHandler(
    filename = function() {
      paste("input1-pf-removed", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_pf1_cleanup(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$download_c2_pf_cleaned_input <- downloadHandler(
    filename = function() {
      paste("input2-pf-removed", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_pf2_cleanup(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  output$download_c3_pf_cleaned_input <- downloadHandler(
    filename = function() {
      paste("input3-pf-removed", ".txt", sep = "")
    },
    content = function(file) {
      write.table(c_pf3_cleanup(), file, sep = "\t", col.names = T, row.names = F, quote = F)
    }
  )
  
  observe({
    if(!is.null(c_pf_db())){
      if(!is.null(c_pd1())){
        d <- c_orig_pd1()
        d_col <- colnames(d)
        if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
           "rep1" %in% d_col & "rep2" %in% d_col){
          shinyjs::hide("download_c1_pf_cleaned_input")
        } else if("rep1" %in% d_col & "rep2" %in% d_col){
          shinyjs::show("download_c1_pf_cleaned_input")
        } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col) {
          shinyjs::hide("download_c1_pf_cleaned_input")
        }
      }
    } else if(is.null(c_pf_db())){
      shinyjs::hide("download_c1_pf_cleaned_input")
    }
  })
  
  observe({
    if(!is.null(c_pf_db())){
      if(!is.null(c_pd2())){
        d <- c_orig_pd2()
        d_col <- colnames(d)
        if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
           "rep1" %in% d_col & "rep2" %in% d_col){
          shinyjs::hide("download_c2_pf_cleaned_input")
        } else if("rep1" %in% d_col & "rep2" %in% d_col){
          shinyjs::show("download_c2_pf_cleaned_input")
        } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col) {
          shinyjs::hide("download_c2_pf_cleaned_input")
        }
      }
    } else if(is.null(c_pf_db())){
      shinyjs::hide("download_c2_pf_cleaned_input")
    }
  })
  
  observe({
    if(!is.null(c_pf_db())){
      if(!is.null(c_pd3())){
        d <- c_orig_pd3()
        d_col <- colnames(d)
        if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col &
           "rep1" %in% d_col & "rep2" %in% d_col){
          shinyjs::hide("download_c3_pf_cleaned_input")
        } else if("rep1" %in% d_col & "rep2" %in% d_col){
          shinyjs::show("download_c3_pf_cleaned_input")
        } else if("logFC" %in% d_col & "FDR" %in% d_col & "pvalue" %in% d_col) {
          shinyjs::hide("download_c3_pf_cleaned_input")
        }
      }
    } else if(is.null(c_pf_db())){
      shinyjs::hide("download_c3_pf_cleaned_input")
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown1)){
      df <- c_in_pd1()
      if("accession_number" %in% colnames(df)){
        shinyjs::enable("c1_download_mapped_uniprot")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown1)){
      df <- c_in_pd1()
      if("gene" %in% colnames(df)){
        shinyjs::disable("c1_download_mapped_uniprot")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown1)){
      df <- c_in_pd1()
      if("rep1" %in% colnames(df) & "rep2" %in% colnames(df)){
        shinyjs::enable("c1_download_replications_calculated")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown1)){
      df <- c_in_pd1()
      if("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df)){
        shinyjs::disable("c1_download_replications_calculated")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown2)){
      df <- c_in_pd2()
      if("accession_number" %in% colnames(df)){
        shinyjs::enable("c2_download_mapped_uniprot")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown2)){
      df <- c_in_pd2()
      if("gene" %in% colnames(df)){
        shinyjs::disable("c2_download_mapped_uniprot")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown2)){
      df <- c_in_pd2()
      if("rep1" %in% colnames(df) & "rep2" %in% colnames(df)){
        shinyjs::enable("c2_download_replications_calculated")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown2)){
      df <- c_in_pd2()
      if("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df)){
        shinyjs::disable("c2_download_replications_calculated")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown3)){
      df <- c_in_pd3()
      if("accession_number" %in% colnames(df)){
        shinyjs::enable("c2_download_mapped_uniprot")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown3)){
      df <- c_in_pd3()
      if("gene" %in% colnames(df)){
        shinyjs::disable("c2_download_mapped_uniprot")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown3)){
      df <- c_in_pd3()
      if("rep1" %in% colnames(df) & "rep2" %in% colnames(df)){
        shinyjs::enable("c3_download_replications_calculated")
      }
    }
  })
  
  observe({
    if(!is.null(input$c_file_pulldown3)){
      df <- c_in_pd3()
      if("logFC" %in% colnames(df) & "FDR" %in% colnames(df) & "pvalue" %in% colnames(df)){
        shinyjs::disable("c3_download_replications_calculated")
      }
    }
  })
  
  observe({
    if (is.null(input$c_file_pulldown1)){
      shinyjs::disable("c1_download_mapped_uniprot")
      shinyjs::disable("c1_download_replications_calculated")
      shinyjs::disable("c_download_snp_to_genes")
      shinyjs::disable("c_download_basic_plots")
      shinyjs::disable("c_download_protein_comparisons")
      shinyjs::disable("c_download_snp")
      shinyjs::disable("c_download_goi")
      shinyjs::disable("c_download_inweb")
      shinyjs::disable("c_download_protein_fams")
    } else {
      shinyjs::enable("c_download_basic_plots")
    }
  })
  
  observe({
    if (is.null(input$c_file_pulldown2)){
      shinyjs::disable("c2_download_mapped_uniprot")
      shinyjs::disable("c2_download_replications_calculated")
      shinyjs::disable("c_download_snp_to_genes")
      shinyjs::disable("c_download_protein_comparisons")
      shinyjs::disable("c_download_snp")
      shinyjs::disable("c_download_goi")
      shinyjs::disable("c_download_inweb")
      shinyjs::disable("c_download_protein_fams")
    } else {
      shinyjs::enable("c_download_protein_comparisons")
    }
  })
  
  observe({
    if (is.null(input$c_file_pulldown3)){
      shinyjs::disable("c3_download_mapped_uniprot")
      shinyjs::disable("c3_download_replications_calculated")
      shinyjs::disable("c_download_snp_to_genes")
      shinyjs::disable("c_download_snp")
      shinyjs::disable("c_download_goi")
      shinyjs::disable("c_download_inweb")
      shinyjs::disable("c_download_protein_fams")
    }
  })
  
  observe({
    if (is.null(input$c_file_SNP)){
      shinyjs::disable("c_download_snp_to_genes")
      shinyjs::disable("c_download_snp")
    } else {
      shinyjs::enable("c_download_snp_to_genes")
      shinyjs::enable("c_download_snp")
    }
  })
  
  observe({
    if (is.null(input$c_file_genes)){
      shinyjs::disable("c_download_goi")
    } else {
      shinyjs::enable("c_download_goi")
    }
  })
  
  observe({
    if (input$c_make_plot_inweb == 0 || is.null(input$c_make_plot_inweb)){
      shinyjs::disable("c_download_inweb")
    } else {
      shinyjs::enable("c_download_inweb")
    }
  })
  
  observe({
    if (is.null(input$c_file_pulldown2)){
      shinyjs::disable("c_download_protein_fams")
    } else {
      if(!is.null(c_compare2_pfe_plot())){
        shinyjs::enable("c_download_protein_fams")
      }
    }
  })

  ##### GENERAL #####
  #documentation
  output$documentation <- renderImage({
    return(list(
      src = "documentation/documentation.png",
      contentType = "image/png",
      width = 1000,
      alt = "savehowto")
    )
  }, deleteFile = FALSE)
  # output$documentation <- renderUI({
  #   return(includeHTML("documentation/doc_0316.html")
  #   )
  # })
  
})



