


# upload file button



ui <- fluidPage(
  fluidRow(
    column(6, 
           parse_file_upload_button_ui("upload1", "Upload any file!"),
           #plot_output("upload1") 
           uiOutput('file_upload_x1'),
           plotlyOutput('protein_comparison_volcano'),
           tableOutput('table_ui')
    ),
    column(6, 
           parse_file_upload_button_ui("upload2", "Upload any file!"),
           uiOutput('file_upload_x2')
           #plot_output("upload2")
    )
  )
)

server <- function(input, output, session) {
  
  x1 = reactiveValues()
  
  x1 = callModule(parse_file, "upload1", color_indv_sig = 'green', color_indv_insig = 'grey')
  x2 = callModule(parse_file, "upload2", color_indv_sig = 'cyan', color_indv_insig = 'grey')
  
  y1 = callModule(select_enriched, "enriched1", pulldown = x1())
  
  #master_select_enriched <- reactiveValues()
  master_select_enriched <- reactive({
    y1 = callModule(select_enriched, "enriched1", pulldown = x1())
    #y2 = callModule(select_enriched, "enriched2", pulldown = x2())
    
    return(y1())
    #if (all(!is.null(y1()) & !is.null(y2()))) browser()
    
    #return(list(y1(), y2()))
  })
  
  #intersect <- reactive({
  #  master = master_select_enriched()
  #  genes = lapply(master, function(x) x[x$significant,]$gene)
  #  return(Reduce(intersect, genes))
  #})
  
  
  #callModule(plot_protein_comparison, "plot1", pulldown = master_select_enriched())

  # basic volcano plot
  a_vp_gg <- reactive({
    d <- y1() #master_select_enriched()
    p <- plot_volcano_basic(d)
    p <- plot_overlay(p, as.bait('BCL2')) # add bait
    print(head(d))
    return(p)
  })
  
  # basic volcano plot
  a_vp <- reactive({
    p <- a_vp_gg()
    p <- make_interactive(p, legend = T)
    #if (input$a_goi_search_rep != '') p <- add_markers_search(p, a_search_gene())
    #p <- add_hover_lines_volcano(p, line_pvalue = input$a_pval_thresh, line_logfc = input$a_logFC_thresh, logfc_direction = input$a_logfc_direction, sig_type = input$a_significance_type)
    #p <- add_layout_html_axes_volcano(p, 500*0.8, 625*0.8)
    return(p)
  })
  
  # output volcano
  output$protein_comparison_volcano <- renderPlotly({
    a_vp()
  })
  
  
  
  
  
  output$file_upload_x1 <- renderUI({
    select_enriched_ui('enriched1', logfc_limit = 25)
  })
  
  output$file_upload_x2 <- renderUI({
    select_enriched_ui('enriched2', logfc_limit = 25)
  })
  
  
  
  
  
  
  
  ###
  output$table_ui <- renderTable({
    qq = master_select_enriched()
    qq
  })
  
  
  
  
  
  
}

shinyApp(ui, server)
