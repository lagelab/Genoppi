
statsParamsOptions <- function(id) {
  conditionalPanel("input.sidebarmenu === 'dashboard'",
    uiOutput(NS(id, "modTTest")),
    uiOutput(NS(id, "signifType")),
    uiOutput(NS(id, "fdrThresh")),
    uiOutput(NS(id, "pValThresh")),
    uiOutput(NS(id, "logfcThresh")),
    uiOutput(NS(id, "logfcDir")),
  )
}

# for renderUI, specify namespace by ns <- session$ns, then use ns("<inputName>")
# for details, see: 
#   Using renderUI with modules section on 
#   https://shiny.rstudio.com/articles/modules.html
statsParamsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$modTTest <- renderUI({
      ns <- session$ns
      radioButtons(
        ns("modTTestIn"), 'Select type of moderated t-test', 
        c("One sample", "Two sample"),
        selected = "Two sample", inline = T
      )
    })
    output$signifType <- renderUI({
      ns <- session$ns
      radioButtons(
        ns("signifTypeIn"), "Significance metric", 
        choiceNames = list("FDR", HTML("<i>P</i>-value")),
        choiceValues = list("fdr",'pvalue'), inline = T
      )
    })
    output$fdrThresh <- renderUI({
      ns <- session$ns
      sliderInput(ns("fdrThreshIn"), "FDR threshold",
                  min = 0, max = 1, value = 0.1, step = 0.01)
    })
    output$pValThresh <- renderUI({
      ns <- session$ns
      sliderInput(ns("pValThreshIn"), "P-Value threshold",
                  min = 0, max = 1, value = 0.1, step = 0.01)
    })
    output$logfcThresh <- renderUI({
      ns <- session$ns
      # TODO
      # if(!is.null(a_file_pulldown_r() )){
      #   limit <- calc_logfc_limit(a_pulldown(), input$a_logfc_direction)
      #   sliderInput("a_logFC_thresh", HTML("log<sub>2</sub>FC threshold"),
      #               min = 0, max = limit, value = 0, step = 0.1)
      # }else
      sliderInput(ns("logfcThreshIn"), HTML("log<sub>2</sub>FC threshold"),
                  min = 0, max = 1, value = 0, step = 0.1)
    })  
    output$logfcDir <- renderUI({
      ns <- session$ns
      radioButtons(ns("logfcDirIn"), HTML("log<sub>2</sub>FC direction"),
                   c("Neg" = "negative", "Both" = "both","Pos" = "positive"),
                   selected = 'positive',
                   inline = T)
    })
    
    return(
      reactive({
        list(modTTest=input$modTTestIn,
             signifType=input$signifTypeIn,
             fdrThresh=input$fdrThreshIn,
             pvalThresh=input$pValThreshIn,
             logfcThresh=input$logfcThreshIn,
             logfcDir=input$logfcDirIn
             )
      })
    )
    # return(reactiveValues(modTTest=reactive(input$modTTestIn),
    #                       signifType=reactive(input$signifTypeIn),
    #                       fdrThresh=reactive(input$fdrThreshIn),
    #                       pvalThresh=reactive(input$pValThreshIn),
    #                       logfcCutOff=reactive(input$logfcCutOffIn),
    #                       logfcDir=reactive(input$logfcDirIn)))
  })
}

#### Moderated t-test Type selection ####
# modTTestServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     output$modTTest <- renderUI({
#       radioButtons(
#         id, 'Select type of moderated t-test', c("One sample", "Two sample"),
#         selected = "Two sample", inline = T
#       )
#     })
#   })
# }

#### Significance Type selection ####
# modTTestRadioBtn <- function(id) {
#   uiOutput(NS(id, "signifType"))
# }

# modTTestServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     output$signifType <- renderUI({
#       radioButtons(
#         id, 'Select type of moderated t-test', c("One sample", "Two sample"),
#         selected = "Two sample", inline = T
#       )
#     })
#   })
# }

#### P-Value slider ####
# pValSlider <- function(id) {
#   uiOutput(NS(id, "pValThresh"))
# }

# pValServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     output$pValThresh <- renderUI({
#       sliderInput(id, HTML("<i>P</i>-value threshold"),
#                   min = 0, max = 1, value = 0.05, step = 0.001)
#     })
#   })
# }

