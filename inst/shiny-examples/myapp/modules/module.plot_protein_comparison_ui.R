


plot_protein_comparison_ui <- function(id, label = "Upload a file hey") {
  ns <- NS(id)
  plotlyOutput(ns("protein_comparison_volcano"))
}