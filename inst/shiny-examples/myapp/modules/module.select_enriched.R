



select_enriched <- function(input, output, session, pulldown) {
  
  # map pulldown to significant
  pulldown_significant <- reactive({
    d = pulldown
    
    if (input$a_significance_type == 'fdr'){
      d1 = id_enriched_proteins(d, fdr_cutoff = input$a_fdr_thresh, 
                                logfc_dir = input$a_logfc_direction, 
                                logfc_cutoff = input$a_logFC_thresh)
    } else {
      d1 = id_enriched_proteins(d, fdr_cutoff = NULL, 
                                p_cutoff = input$a_pval_thresh, 
                                logfc_dir = input$a_logfc_direction, 
                                logfc_cutoff = input$a_logFC_thresh)
    }
    print(sum(d$significant))
    return(d1)
  })
  
  return(pulldown_significant)
}
  
  
  
  
  
