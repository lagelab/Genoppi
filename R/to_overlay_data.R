#' @title to overlay data
#' @description Adds columns to a data.frame (e.g. one used for overlay)
#' that are required for downstream functionality in genoppi/
#' @param df a data.frame
#' @param dataset optional string, if dataset is not provided in df.
#' @param rm.sig boolean. should signicicant items be removed?
#' @export
#' @family interactive

to_overlay_data <- function(df, dataset=NULL, rm.sig = F) {
  cnames = colnames(df)
  
  # remove all non-significant rows
  if (!is.null(df$significant) & rm.sig){
    df <- df[df$significant, ]
  }
  
  # if the following columns are not specified in the reference
  # they are set to the default in this function.
  if ('dataset' %nin% cnames) df$dataset = dataset
  if ('label' %nin% cnames) df$label <- TRUE
  if ('stroke' %nin% cnames) df$stroke <- TRUE
  if ('col_significant' %nin% cnames) df$col_significant <- 'yellow'
  if ('col_other' %nin% cnames) df$col_other <- 'grey'
  if ('col_border' %nin% cnames) df$col_border <- 'black'
  if ('alt_label' %nin% cnames) df$alt_label <- NA
  if ('label_size' %nin% cnames) df$label_size = 3
  if ('pLI' %nin% cnames) df$pLI <- NA
  if ('shape' %nin% cnames) df$shape <- 21
  if ('opacity' %nin% cnames) df$opacity <- 1
  #if ('gg.size' %nin% cnames) df$gg.size <- 3.5 # deprecated
  if ('size_gg' %nin% cnames) df$size_gg <- 3.5
  if ('gene' %nin% cnames) df$gene <- NA
  if ('size' %nin% cnames) df$size <- 9
  if ('legend_order' %nin% cnames) df$legend_order <- NA 
  if ('symbol' %nin% cnames) df$symbol <- shape_to_symbol(df$shape)
  #if ('symbol' %nin% cnames) df$symbol <- 'circle'
  
  return(as.data.frame(df))
}
