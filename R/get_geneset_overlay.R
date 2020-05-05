#' @title plot pathways
#' @description generates a list of data.frames that can be inputted to \code{plot_overlay()}
#' @param pulldown pulldown data
#' @param database see get_pathways
#' @param k integer. Up to how many of the most recurrent gene sets should be displayed at once?
#' @note this is a work in progress.
#' @examples 
#' \dontrun{
#' data("example_data")
#' df = example_data %>% 
#'    calc_mod_ttest() %>% 
#'    id_enriched_proteins()
#'    
#' plot_volcano_basic(df) %>% 
#'    plot_overlay(get_geneset_overlay(df, 'hgnc', k = 25)) %>% 
#'    make_interactive()
#' }
#' @keywords internal

get_geneset_overlay <- function(data, database, k=100){
  
  # check inpug
  stopifnot('gene' %in% colnames(data))
  
  # get pathways
  pathways = get_pathways(database, data$gene)
  
  # merge pulldown, pathways and colors
  frequencies <- assign_freq(pathways, 'pathway')
  colors <- assign_color(pathways, 'pathway')
  overlay = merge(frequencies, data)
  overlay = merge(colors, overlay)
  
  # parameters for plotting
  overlay$dataset = overlay$pathway
  overlay$alt_label = overlay$pathway
  overlay$size = overlay$Freq/max(overlay$Freq)
  overlay$size = 9+exp(3*overlay$size)
  overlay$gg.size = overlay$size/2
  overlay$col_significant = overlay$color
  overlay$col_other = overlay$color
  overlay$symbol = 'square'
  overlay$shape = 22 # square with outline // note plotly does not like this.
  overlay$opacity = 0.8
  overlay$label = FALSE 
  
  # subset pathways for lowest allowed frequency
  
  overlay = overlay[overlay$Freq >= lowest_allowed_freq,]
  
  # only significant things are plotted
  overlay = validate_reference(overlay[overlay$significant,], warn = F)
  overlay$legend_order = rev(order(overlay$size))
  return(list(geneset=overlay))
  
}



