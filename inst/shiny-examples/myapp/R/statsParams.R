statsParamsValues <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactiveValues(
      # initial values, values changed by UI afterwards
      modTTest="Two sample",
      signifType="fdr",
      fdrThresh=0.1,
      pValThresh=NULL,
      logfcThresh=0,
      logfcDir="positive"
      ))
  })
}


statsParamsOptions <- function(id) {
  ns <- NS(id)
  box(
    title = "Statistics Parameters", width = NULL, solidHeader = TRUE, 
    status = "primary", collapsible = TRUE, collapsed = FALSE,
    fluidRow(column(6, uiOutput(NS(id, "modTTest"))),
             column(6, uiOutput(NS(id, "modTTestNotif")))),
    fluidRow(column(12, uiOutput(NS(id, "logfcThresh")))),
    fluidRow(column(6, uiOutput(NS(id, "logfcDir"))),
             column(6, uiOutput(NS(id, "signifType")))),
    fluidRow(conditionalPanel(condition = "input.signifTypeIn == 'fdr'",
                              ns = ns,
                              column(12, uiOutput(NS(id, "fdrThresh"))))),
    fluidRow(conditionalPanel(condition = "input.signifTypeIn == 'pvalue'", 
                              ns = ns,
                              column(12, uiOutput(NS(id, "pValThresh")))))
  )
}

# for renderUI, specify namespace by ns <- session$ns, then use ns("<inputName>")
# for details, see: 
#   Using renderUI with modules section on 
#   https://shiny.rstudio.com/articles/modules.html
statsParamsServer <- function(id, 
                              mapAccessionToGeneServer, 
                              dataServer, 
                              columnsValues,
                              statsParamsValues) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(mapAccessionToGeneServer()$format, {
      d <- mapAccessionToGeneServer()$data
      fmtCk <- mapAccessionToGeneServer()$format$check
      req(d, fmtCk)
      choices <- NULL
      if (fmtCk$gene_rep|fmtCk$accession_rep) {
        choices <- c(choices, "One sample")
      }
      if (fmtCk$gene_sample_control|fmtCk$accession_sample_control) {
        choices <- c(choices, "Two sample")
      }
      if (length(choices) == 0) {
        output$modTTest <- renderUI({
           HTML(paste0(
            bold('Note:  '),
            "No moderated t-test can be performed,
            not enough rep/sample/control columns found"))})
        columnsValues$mod_ttest_columns <- list()
      } 
      else {
        if ("Two sample" %in% choices) {
          selected="Two sample"
          used_columns <- colnames(d)[
            (grepl('^sample[0-9]$', colnames(d))|grepl('^control[0-9]$', colnames(d)))]
        } else if ("One sample" %in% choices) {
          selected="One sample"
          used_columns <- colnames(d)[grepl('^rep[0-9]$', colnames(d))]
        } else {
          stop("No valid modTTest type can be selected in statsParamsServer")}
        output$modTTest <- renderUI({
          radioButtons(
            ns("modTTestIn"), 'Select type of moderated t-test',
            choices,
            selected = selected, inline = T)})
        statsParamsValues$modTTest <- selected
        columnsValues$mod_ttest_columns <- as.list(used_columns)

        unavailable_choice <- c("One sample", "Two sample")
        unavailable_choice <- unavailable_choice[!(unavailable_choice %in%  choices)]
        if (length(unavailable_choice) > 0) {
          needed_columns <- list("One sample"=">2 rep columns",
                                 "Two sample"=">2 sample columns & >2 control columns")
          needed_columns <- needed_columns[unavailable_choice]
          output$modTTestNotif <- renderUI({
            HTML(c(paste(unavailable_choice, collapse=" and"),
                   "moderated t-test not available,",
                   paste(needed_columns, collapse = " and "),
                   "are needed respectively"))})
        }
      }
    })
    
    output$signifType <- renderUI({
      radioButtons(
        ns("signifTypeIn"), "Significance metric", 
        choiceNames = list("FDR", HTML("<i>P</i>-value")),
        choiceValues = list("fdr",'pvalue'), inline = T,
        selected = "fdr"
      )
    })
    output$fdrThresh <- renderUI({
      sliderInput(ns("fdrThreshIn"), "FDR threshold",
                  min = 0, max = 1, value = 0.1, step = 0.01)
    })
    output$pValThresh <- renderUI({
      sliderInput(ns("pValThreshIn"), "P-Value threshold",
                  min = 0, max = 1, value = 0.1, step = 0.01)
    })
    
    observeEvent(dataServer(), {
      if(!is.null(dataServer())){
        logfc_limit <- calc_logfc_limit(
          dataServer(), statsParamsValues$logfcDir)
      } else {
        logfc_limit <- 1
      }
      output$logfcThresh <- renderUI({
        sliderInput(ns("logfcThreshIn"), HTML("log<sub>2</sub>FC threshold"),
                    min = 0, max = logfc_limit, value = 0, step = 0.1)})
      }
    )  
    
    output$logfcDir <- renderUI({
      radioButtons(ns("logfcDirIn"), HTML("log<sub>2</sub>FC direction"),
                   c("Neg" = "negative", "Both" = "both","Pos" = "positive"),
                   selected = 'positive',
                   inline = T)
    })
    
    observeEvent(input$modTTestIn, {
      statsParamsValues$modTTest<-input$modTTestIn
      d <- mapAccessionToGeneServer()$data
      req(d)
      if (statsParamsValues$modTTest=="Two sample") {
        used_columns <- colnames(d)[
          (grepl('^sample[0-9]$', colnames(d))|grepl('^control[0-9]$', colnames(d)))]
      } else if (statsParamsValues$modTTest=="One sample") {
        used_columns <- colnames(d)[grepl('^rep[0-9]$', colnames(d))]
      }
      else {stop(
        "Invalid statsParamsValues modTTest value detected in statsParamsServer.")
      }
      columnsValues$mod_ttest_columns <- as.list(used_columns)
    })
    observeEvent(input$signifTypeIn, {
      statsParamsValues$signifType<-input$signifTypeIn})
    observeEvent(input$fdrThreshIn, {
      statsParamsValues$fdrThresh<-input$fdrThreshIn})
    observeEvent(input$pValThreshIn, {
      statsParamsValues$pValThresh<-input$pValThreshIn})
    observeEvent(input$logfcThreshIn, {
      statsParamsValues$logfcThresh<-input$logfcThreshIn})
    observeEvent(input$logfcDirIn, {
      statsParamsValues$logfcDir<-input$logfcDirIn})
  })
}

