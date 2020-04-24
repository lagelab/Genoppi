#' @title parse an input file in shiny
#' @description reatives that will parse an input file




parse_file <- function(input, output, session,
                       color_indv_sig='green', color_indv_insig='grey') {
  
  pulldown <- eventReactive(input$file_upload_ui,{
    req(input$file_upload_ui)
    file = input$file_upload_ui$datapath
    print(file)
    #file = input_file$datapath
    data = read_input(file, sep = '\t', header = T)
    return(data)
  })
  
  # map accession numbers to genes
  pulldown_mapped <- reactive({
    req(pulldown())
    pulldown <- pulldown()
    if (pulldown$format$check$accession_rep == TRUE){
      pulldown$data <- map_gene_id(pulldown$data)
    }
    return(pulldown$data)
  })  
  
  # pulldown ttest
  pulldown_ttest <- reactive({
    req(pulldown_mapped())
    pulldown <- pulldown_mapped()
    pulldown <- calc_mod_ttest(pulldown)
    #browser()
    return(pulldown)
  })
  
  return(pulldown_ttest)
  
}