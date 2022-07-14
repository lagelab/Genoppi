select_mod_ttest_ui <- function(id) {
  uiOutput(NS(id, "TTEST_type"))
}

select_mod_ttest_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 output$TTEST_type <- renderUI({
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