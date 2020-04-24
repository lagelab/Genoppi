
parse_file_upload_button_ui <- function(id, label = "Upload a file") {
  ns <- NS(id)
  fileInput(ns("file_upload_ui"), label,
  accept = c(
     'text/csv',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      '.csv',
      '.tsv'))
}



