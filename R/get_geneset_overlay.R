#' @title get geneset as overlay
#' @description generates a list of data.frames that can be inputted to \code{plot_overlay()}
#' @param data pulldown data
#' @param database What database should be used for the search? For available pathways, see \code{?get_pathways}.
#' @param k integer or NULL. Up to how many of the most recurrent gene sets should be displayed at once? NULL will
#' indicate that no gene sets should be subsetted at all.
#' @param only.significant boolean. If true, only column with significant=TRUE will be considered.
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
#' @export

get_geneset_overlay <- function(data, database, k=100, only.significant = T){
  
  # check the format of the input
  stopifnot('gene' %in% colnames(data))

  # subset data
  if (only.significant){
    stopifnot('significant' %in% colnames(data))
    data = data[data$significant, ]
  }
  
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
  if (!is.null(k)){
    tabl = calc_cumsum_table(overlay, 'pathway')
    lowest_allowed_freq = min(tabl$Freq[tabl$n <= k])
    overlay = overlay[overlay$Freq >= lowest_allowed_freq,]
  }

  # only significant things are plotted
  overlay$legend_order = rev(order(overlay$size))
  return(list(geneset=overlay))
  
}



