select_enriched_ui <- function(id, logfc_limit){
  ns <- NS(id)
  #tagList(
    fluidRow(
           column(12,
              radioButtons(ns("a_significance_type"), "Significance metric",
                           choiceNames = list("FDR", HTML("<i>P</i>-value")),
                           choiceValues = list("fdr",'pvalue'),
                           inline = T),
              sliderInput(ns("a_fdr_thresh"), "FDR threshold",
                          min = 0, max = 1, value = 0.1, step = 0.01),
              sliderInput(ns("a_pval_thresh"), HTML("<i>P</i>-value threshold"),
                          min = 0, max = 1, value = 0.05, step = 0.001),
              radioButtons(ns("a_logfc_direction"), HTML("log<sub>2</sub>FC direction"),
                           c("Neg" = "negative", 
                             "Both" = "both",
                             "Pos" = "positive"),
                           selected = 'positive',
                           inline = T),
              sliderInput(ns("a_logFC_thresh"), HTML("log<sub>2</sub>FC threshold"),
                          min = 0, max = logfc_limit, value = 0, step = 0.1)
           )
    )
  #)
}
