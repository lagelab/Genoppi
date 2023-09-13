externalPPIBox <- function(id) {
  box(
    title = "InWeb / iRefIndex / BioPlex", width = NULL, solidHeader = TRUE, 
    status = "primary", collapsible = TRUE, collapsed = FALSE, 
    tagList(img(src='icon_inweb.png',width='20px')),
    fluidRow(
      column(12, 
             uiOutput(NS(id, "ppi_select_ui")))
    ),
    # selected PPI dataset 
    fluidRow(
      column(8, uiOutput(NS(id, "bait_search"))),
      conditionalPanel(condition = "input.selected_ppi == 'inweb'", ns=NS(id),
                       column(4, uiOutput(NS(id, "inweb_type_ui")))),
      conditionalPanel(condition = "input.selected_ppi == 'irefindex'", ns=NS(id),
                       column(4, uiOutput(NS(id, "irefindex_min_pub_ui")),)),
      conditionalPanel(condition = "input.selected_ppi == 'bioplex'", ns=NS(id),
                       column(4, uiOutput(NS(id, "bioplex_prob_ui")))),
    ),
    # footer
    fluidRow(
      column(4,
             uiOutput(NS(id, "overlay_inweb_ui")),
             uiOutput(NS(id, "a_ppi_message"))),
      column(4, uiOutput(NS(id, "inweb_label_ui"))),
      column(4, shinyjs::hidden(myDownloadButton(
        NS(id, "a_ppi_mapping_df_download"),'Mapping', 
        img=icon('file-alt', lib = "font-awesome"))))
    ),
    fluidRow(
      column(12, uiOutput(NS(id, "info_inweb_ui")))
    )
  )
}

overlayExternalPPIServer <- function(id, enrichmentStatsServer, a_ppi_mapping_df) {
  if (!is.reactive(enrichmentStatsServer)) {
    stop("enrichmentStatsServer passed into overlayExternalPPIServer is not reactive")}
  if (!is.reactive(a_ppi_mapping_df)) {
    stop("a_ppi_mapping_df passed into overlayExternalPPIServer is not reactive")}
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$bait_search <- renderUI({
      textInput(
        ns("bait_search"), value = "", 
        "Input HGNC symbol to search for protein interactors (e.g. ZBTB7A)")
    })
    
    output$ppi_select_ui <- renderUI({
      selectInput(
        ns('selected_ppi'), 'select PPI database',  
        c("InWeb_InBioMap" = "inweb", 
          "iRefIndex 17.0" = "irefindex", 
          "BioPlex 3.0" = "bioplex"),
        multiple=F, selectize=TRUE, selected = "grey")
    })
    
    output$inweb_type_ui <- renderUI({
      selectInput(
        ns('inweb_type'), 'Select interactor type', 
        c("All" = 'all',"High-confidence" = 'hc',"Gold-standard" = 'gs'))
    })
    
    output$irefindex_min_pub_ui <- renderUI({
      sliderInput(
        ns('irefindex_min_pub'), 'Select min. publications',
        min = 1, max = max(irefindex_table$Score.np.max), 
        value = 2, step = 1)
    })
    
    output$bioplex_prob_ui <- renderUI({
      sliderInput(
        ns('bioplex_prob'), 'Select bioplex probability', 0, 1, 0.9, 0.01)
    })
    
    output$overlay_inweb_ui <- renderUI({
      # TODO implement validate and check validation correctness of validation
      # original: validate(need(a_file_pulldown_r()  != '', ""))
      # continue: validate(need(dataPathS()  != '', "dataPathS is empty"))
      checkboxInput(
        ns("overlay_inweb"), label = "Toggle overlay", value = TRUE)
    })
    
    output$inweb_label_ui <- renderUI({
      # validate(need(a_file_pulldown_r()  != '', ""))
      # validate(need(dataPathS()  != '', ""))
      checkboxInput(ns("inweb_label"), label = "Toggle labels", value = TRUE)
    })
    
    output$info_inweb_ui <- renderUI({
      actionLink(ns('info_inweb'), ' ', icon = icon('question-circle'))})
    
    observeEvent(input$info_inweb,{
      text = paste(readLines(genoppi:::find_docs('inweb_table.info')), '<br> <br>',
                   readLines(genoppi:::find_docs('irefindex_table.info')), '<br> <br>',
                   readLines(genoppi:::find_docs('bioplex_table.info')))
      showModal(modalDialog(HTML(text), easyClose = T,
                            footer=genoppi.ver, title = 'InWeb'))
    })
    
    output$a_ppi_mapping_df_download <- downloadHandler(
      filename = function() {
        paste("genoppi-inweb-mapping",".csv", sep="")
      },
      content = function(file) {
        pulldown = enrichmentStatsServer()
        inweb = a_ppi_mapping_df()[,c("dataset","gene")]
        mymerge = merge(pulldown, inweb, by = 'gene')
        write.csv(mymerge, file, row.names = F)
      }
    )
    
    observeEvent(input$bait_search, {
      req(enrichmentStatsServer())
      shinyjs::toggle(
        "a_ppi_mapping_df_download", 
        condition=any(input$bait_search%in%c(inweb_table$Gene1,inweb_table$Gene2))
      )}
    )

    return(
      reactive({
        list(bait_search=input$bait_search,
             overlay_inweb=input$overlay_inweb,
             selected_ppi=input$selected_ppi,
             inweb_type=input$inweb_type,
             irefindex_min_pub=input$irefindex_min_pub,
             bioplex_prob=input$bioplex_prob,
             inweb_label=input$inweb_label)
      })
    )
  })
}