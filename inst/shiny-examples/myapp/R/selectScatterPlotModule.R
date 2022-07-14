select_scatterplot_ui <- function(id) {
  uiOutput(NS(id, "select_scatterplot_ui"))
}

select_scatterplot_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 output$select_scatterplot_ui <- renderUI({
                   radioButtons(
                     id,
                     'Select type of moderated t-test',
                     c("One sample", "Two sample"),
                     selected = "Two sample",
                     inline = T
                   )
                 })
               })
}