plotVennDiagram <- function(id) {
  box(
    title = tagList(img(src='icon_venn_a.png',width='22px'), "Venn diagram"),
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
             uiOutput(NS(id,"multifile_select_venn_set"))),
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

plotVennServer <- function(id, 
                           multi_sigS1, 
                           multi_sigS2, 
                           multi_sigS3, 
                           multi_FileName1,
                           multi_FileName2,
                           multi_FileName3,
                           plotValues, 
                           vennDfS) {
  if (!is.reactive(multi_sigS1)){
    stop("multi_sigS1 passed to plotVennServer is not reactive")}
  if (!is.reactive(multi_sigS2)){
    stop("multi_sigS2 passed to plotVennServer is not reactive")}
  if (!is.reactive(multi_sigS3)){
    stop("multi_sigS3 passed to plotVennServer is not reactive")}
  if (!is.reactive(multi_FileName1)){
    stop("multi_FileName1 passed to plotVennServer is not reactive")}
  if (!is.reactive(multi_FileName2)){
    stop("multi_FileName2 passed to plotVennServer is not reactive")}
  if (!is.reactive(multi_FileName3)){
    stop("multi_FileName3 passed to plotVennServer is not reactive")}
  if (!is.reactivevalues(plotValues)){
    stop("plotValues passed to overlayVolcanoServer is not reactiveValues")}
  if (!is.reactive(vennDfS)) {
    stop("vennDfS passed to plotVennServer is not reactive")}
  moduleServer(id, function(input, output, session){
    observeEvent(c(multi_sigS1(), 
                   multi_sigS2(),
                   multi_sigS3(),
                   multi_FileName1(), 
                   multi_FileName2(),
                   multi_FileName3(),
                   input$venn_set), {
      servers <- c(multi_sigS1, multi_sigS2, multi_sigS3)
      # server_bool indicates whether each server is null
      server_bool <- c(!is.null(multi_sigS1()), 
                       !is.null(multi_sigS2()), 
                       !is.null(multi_sigS3()))
      filenames <- c(multi_FileName1, multi_FileName2, multi_FileName3)
      # TODO handle deletion of files, detect when number of files change
      if (sum(server_bool) > 1) {
        set_names <- unlist(lapply(1:length(server_bool), function(server_i){
          if (server_bool[server_i]) {return(filenames[server_i][[1]]())}
        }))
        set_genes <- lapply(1:length(server_bool), function(server_i){
          if (server_bool[server_i]) {
            # to extract the server from a vector, unique to reactiveVal
            sig <- servers[server_i][[1]]()
            return(sig[sig$significant,"gene"])
          }
        })
        # to remove null servers
        set_genes <- set_genes[!sapply(set_genes, is.null)]
        names(set_genes) <- set_names
        all_genes <- Reduce(union, set_genes)
        gene_groups <- lapply(set_genes, function(sig){
          sapply(all_genes, function(g) {is.element(g, sig)})
        })
        names(gene_groups) <- set_names
        
        venn_df <- data.frame(gene = all_genes, gene_groups)
        venn_df$count <- apply(venn_df, 1, function(row) {
          sum(as.logical(row[2:length(row)]))
        })
        set_bool_list <- lapply(input$venn_set, function(set){as.logical(venn_df[[set]])})
        filter_vec <- Reduce(function(set_a, set_b){set_a & set_b}, 
                             c(set_bool_list, rep(T, length(all_genes))))
        filtered_venn_df <- venn_df[filter_vec,]
        vennDfS(filtered_venn_df)
        output$venn_df <- DT::renderDataTable({
          req(filtered_venn_df)
          DT::datatable(filtered_venn_df, 
                        rownames = FALSE, 
                        colnames = c("Gene", set_names, "Overlap count"))
        })
        print(set_genes)
        venn <- ggVennDiagram::Venn(set_genes)
        data <- ggVennDiagram::process_data(venn)
        # print(data)
        plotValues$venn <- ggplot2::ggplot() +
          ggplot2::geom_sf(ggplot2::aes(fill = count), data = ggVennDiagram::venn_region(data)) +
          ggplot2::geom_sf_text(ggplot2::aes(label = name), data = ggVennDiagram::venn_setlabel(data)) +
          ggplot2::geom_sf_label(ggplot2::aes(label = count), data = ggVennDiagram::venn_region(data)) +
          ggplot2::theme_void()
        # plotValues$venn <- ggVennDiagram(set_genes)
        output$VennDiagram <- renderPlot({
          plotValues$venn
        })
      }
    })
    
    observeEvent(
      c(multi_FileName1(),
      multi_FileName2(),
      multi_FileName3(),
      multi_sigS1(),
      multi_sigS2(),
      multi_sigS3()), {
      # server_bool indicates whether each server is null
      server_bool <- c(!is.null(multi_sigS1()), 
                      !is.null(multi_sigS2()), 
                      !is.null(multi_sigS3()))
      filenames <- c(multi_FileName1(), multi_FileName2(), multi_FileName3())[server_bool]
      output$multifile_select_venn_set <- renderUI({
        selectInput(
          NS(id, "venn_set"), 
          label = "View gene sets in venn diagram (not selecting any sets displays the union of all sets)",
          choices = filenames, multiple = TRUE)
      })
    })
    
    
    output$VennDiagram_download <- downloadHandler(
      filename = 'genoppi-ggvenn-plot.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
          res = 300, units = "in")
        }
        ggplot2::ggsave(
          file,
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