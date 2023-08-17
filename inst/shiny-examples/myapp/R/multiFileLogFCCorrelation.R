logFCCorrelation <- function(id) {
  box(
    title = tagList(img(src='icon_scatter.png',width='22px'), "LogFC correlation"),
    width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE, 
    collapsed = FALSE,
    fluidRow(
      style = "padding-bottom:75px",
      column(12, plotly::plotlyOutput(NS(id, "logfc_correlation")))
    ),
    fluidRow(
      column(8,uiOutput(NS(id,"multifile_select_logfc_cor_pair"))),
    ),
  )
}



logFCCorrelationServer <- function(
  id,
  bothSigColor,
  firstSigColor,
  secondSigColor,
  noSigColor,
  multi_sigS1,
  multi_sigS2,
  multi_sigS3,
  multi_FileName1,
  multi_FileName2,
  multi_FileName3,
  multiFileLogFCCorrelationPlot, 
  multiFileLogFCCorrelationDf) {
  if (!is.reactive(multi_sigS1)){
    stop("multi_sigS1 passed to logFCCorrelationServer is not reactive")}
  if (!is.reactive(multi_sigS2)){
    stop("multi_sigS2 passed to logFCCorrelationServer is not reactive")}
  if (!is.reactive(multi_sigS3)){
    stop("multi_sigS3 passed to logFCCorrelationServer is not reactive")}
  if (!is.reactive(multi_FileName1)){
    stop("multi_FileName1 passed to logFCCorrelationServer is not reactive")}
  if (!is.reactive(multi_FileName2)){
    stop("multi_FileName2 passed to logFCCorrelationServer is not reactive")}
  if (!is.reactive(multi_FileName3)){
    stop("multi_FileName3 passed to logFCCorrelationServer is not reactive")}
  if (!is.reactive(multiFileLogFCCorrelationPlot)){
    stop("multiFileLogFCCorrelationPlot passed to logFCCorrelationServer is not reactive")}
  if (!is.reactive(multiFileLogFCCorrelationDf)) {
    stop("multiFileLogFCCorrelationDf passed to logFCCorrelationServer is not reactive")}
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(c(input$color_both_sig_in,
                  input$color_first_sig_in,
                  input$color_second_sig_in,
                  input$color_insig_in,
                  multi_sigS1(),
                  multi_sigS2(),
                  multi_sigS3(),
                  multi_FileName1(),
                  multi_FileName2(),
                  multi_FileName3(),
                  input$selected_pair), {
                    servers <- c(multi_sigS1, multi_sigS2, multi_sigS3)
                    # server_bool indicates whether each server is null
                    server_bool <- c(!is.null(multi_sigS1()),
                                    !is.null(multi_sigS2()),
                                    !is.null(multi_sigS3()))
                    filenames <- c(multi_FileName1(), 
                                  multi_FileName2(),
                                  multi_FileName3())
                    if (sum(server_bool) >= 2 & !is.null(input$selected_pair)) {
                      pair_choiceValues <- unlist(apply(
                        combn(filenames, 2), 2,
                        function(p){return(list(p))}), recursive = F)
                      selected_pair <- pair_choiceValues[[as.integer(input$selected_pair)]]
                      selected_indices <- sapply(selected_pair, function(selected) {
                        which(filenames == selected)
                      })
                      selected_df <- lapply(selected_indices, function(server_i){
                        return(servers[server_i][[1]]())
                      })   
                      df1 <- selected_df[[1]][c('gene', 'logFC', 'significant')]
                      df2 <- selected_df[[2]][c('gene', 'logFC', 'significant')]
                      names(df1)[2:3] <- c("df1logfc", "df1sig")
                      names(df2)[2:3] <- c("df2logfc", "df2sig")
                      df1_logfc_axis <- paste(filenames[selected_indices[1]], 'logFC')
                      df2_logfc_axis <- paste(filenames[selected_indices[2]], 'logFC')
                      logfc_df <- merge(x=df1, y=df2, by='gene', all.x= T, all.y=T)
                      logfc_df$df1sig <- !is.na(logfc_df$df1sig)
                      logfc_df$df2sig <- !is.na(logfc_df$df2sig)
                      logfc_df$df1logfc[is.na(logfc_df$df1logfc)] <- 0
                      logfc_df$df2logfc[is.na(logfc_df$df2logfc)] <- 0
                      logfc_df$color <- apply(logfc_df, 1, function(row) {
                        if(as.logical(row["df1sig"]) & as.logical(row["df2sig"])) {
                        return("both")
                        } else if (as.logical(row["df1sig"])) {
                        return("first")
                        } else if (as.logical(row["df2sig"])) {
                        return("second")
                        } else {
                        return(input$color_insig_in)
                        }
                      })
                      logfc_df$color <- factor(logfc_df$color)
                      
                      p <- ggplot(data = logfc_df, aes(x = df1logfc, y = df2logfc, colour = color)) +
                      geom_point() + 
                      scale_color_manual(values = c(
                        "both" = input$color_both_sig_in,
                        "first" = input$color_first_sig_in,
                        "second" = input$color_second_sig_in)
                      ) +
                      theme_minimal()
                      ip <- plotly::ggplotly(p)

                      output$logfc_correlation <- plotly::renderPlotly({
                      ip
                      #  plot_ly(type="scatter", mode ="markers", data = logfc_df, x = ~df1logfc, y = ~df2logfc,
                      #          color = ~color, 
                      #         #  colors = c(bothSigColor(), firstSigColor(), secondSigColor(), noSigColor()),
                      #          text = ~paste0("Gene: ", gene, '<br>', 
                      #                        df1_logfc_axis, ": ", df1logfc, '<br>', 
                      #                        df2_logfc_axis, ": ", df2logfc, '<br>')) %>%
                      #    layout(xaxis=list(title=df1_logfc_axis),
                      #           yaxis=list(title=df2_logfc_axis))
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
        if (sum(server_bool) >= 2) {
          filenames <- c(multi_FileName1(), 
                        multi_FileName2(),
                        multi_FileName3())[server_bool]
          pair_choiceNames <- apply(
            combn(filenames, 2), 2,
            function(p){paste(p, collapse = " vs ")})
          output$multifile_select_logfc_cor_pair <- renderUI({
            radioButtons(
              NS(id, "selected_pair"),
              label = "Select two experiments to visualize their logFC correlation.",
              selected = 1,
              choiceNames = pair_choiceNames,
              choiceValues = 1:length(pair_choiceNames))
            # selectInput(
            #   NS(id, "selected_pair"),
            #   selected = pair_choices[1],
            #   label = "Select two experiments to visualize their logFC correlation.",
            #   choices = pair_choices)
          })
        }
      })
    
    
    # output$VennDiagram_download <- downloadHandler(
    #   filename = 'genoppi-ggvenn-plot.png',
    #   content = function(file) {
    #     device <- function(..., width, height) {
    #       grDevices::png(..., width = width, height = height,
    #                      res = 300, units = "in")
    #     }
    #     ggsave(file,
    #            plot =  plotValues$venn,
    #            device = device,
    #            width = global.img.volcano.download.width,
    #            height = global.img.volcano.download.height)
    #   })
    # 
    # output$VennDf_download <- downloadHandler(
    #   filename = function() {
    #     paste0("genoppi-venn-dataframe",".txt")
    #   },
    #   content = function(file) {
    #     write.table(vennDfS(), file, row.names = F, sep="\t", quote = FALSE)
    #   }
    # )
  })
}