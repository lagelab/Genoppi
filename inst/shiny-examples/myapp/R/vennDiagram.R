plotVennDiagram <- function(id) {
  box(
    title = tagList(img(src='icon_scatter.png',width='22px'), "Pairwise scatter plot"),
    width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE, 
    collapsed = FALSE,
    fluidRow(
      column(12, myDownloadButton(NS(id, "VennDiagram_download"), 'Venn Diagram'))
    ),
    fluidRow(style = "padding-bottom:75px",
             column(12, plotOutput(NS(id, "VennDiagram"))) 
    ),
    fluidRow(
      column(8,
             selectInput(
               NS(id, "venn_set"), 
               label = "View gene sets in venn diagram (not selecting any sets displays the union of all sets)",
               choices = c("File1", "File2", "File3"), multiple = TRUE)),
      column(4,
             myDownloadButton(NS(id, "VennDf_download"), 'Overlap data table', icon("download")))
    ),
    fluidRow(
      column(12,
             DT::dataTableOutput(NS(id, 'venn_df'))
      )
    )
  )
}

plotVennServer <- function(id, multi_sigS1, multi_sigS2, multi_sigS3, plotValues, vennDfS) {
  if (!is.reactive(multi_sigS1)){
    stop("multi_sigS1 passed to plotVennServer is not reactive")}
  if (!is.reactive(multi_sigS2)){
    stop("multi_sigS2 passed to plotVennServer is not reactive")}
  if (!is.reactive(multi_sigS3)){
    stop("multi_sigS3 passed to plotVennServer is not reactive")}
  if (!is.reactivevalues(plotValues)){
    stop("plotValues passed to overlayVolcanoServer is not reactiveValues")}
  if (!is.reactive(vennDfS)) {
    stop("vennDfS passed to plotVennServer is not reactive")}
  
  moduleServer(id, function(input, output, session){
    observeEvent(c(multi_sigS1(), multi_sigS2(), multi_sigS3(), input$venn_set), {
      req(multi_sigS1(), multi_sigS2(), multi_sigS3())
      sig1 <- multi_sigS1()
      sig2 <- multi_sigS2()
      sig3 <- multi_sigS3()
      sig1_genes <- sig1[sig1$significant,"gene"]
      sig2_genes <- sig2[sig2$significant,"gene"]
      sig3_genes <- sig3[sig3$significant,"gene"]
      gene <- union(union(sig1_genes, sig2_genes), sig3_genes)
      venn_df <- data.frame(
        gene=gene,
        File1 = sapply(gene, function(g){is.element(g, sig1_genes)}),
        File2 = sapply(gene, function(g){is.element(g, sig2_genes)}),
        File3 = sapply(gene, function(g){is.element(g, sig3_genes)})
      )
      venn_df$count <- apply(venn_df, 1, function(row) {
        sum(as.logical(row[c("File1", "File2", "File3")]))
      })
      set_bool_list <- lapply(input$venn_set, function(set){as.logical(venn_df[[set]])})
      filter_vec <- Reduce(function(set_a, set_b){set_a & set_b}, 
                           c(set_bool_list, rep(T, length(gene))))
      filtered_venn_df <- venn_df[filter_vec,]
      vennDfS(filtered_venn_df)
      output$venn_df <- DT::renderDataTable({
        req(filtered_venn_df)
        DT::datatable(filtered_venn_df, 
                      rownames = FALSE, 
                      colnames = c("Gene", "File 1", "File 2", "File 3", "Overlap count"))
      })
      plotValues$venn <- ggVennDiagram(list(
        File1=sig1_genes, 
        File2=sig2_genes, 
        File3=sig3_genes
      ))
      output$VennDiagram <- renderPlot({
        plotValues$venn
      })
    })
    
    output$VennDiagram_download <- downloadHandler(
      filename = 'genoppi-ggvenn-plot.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        ggsave(file,
               plot =  plotValues$venn,
               device = device,
               width = global.img.volcano.download.width,
               height = global.img.volcano.download.height)
      })
    
    output$VennDf_download <- downloadHandler(
      filename = function() {
        paste0("genoppi-venn-dataframe",".txt")
      },
      content = function(file) {
        write.table(vennDfS(), file, row.names = F, sep="\t", quote = FALSE)
      }
    )
  })
}