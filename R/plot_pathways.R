#' @title plot pathways
#' @description similar to \code{?plot_overlay}.
#' @param pulldown pulldown data
#' @param database see get_pathways
#' @keywords internal
#' 
#' 

as.geneset <- function(data, database){
  
  # check inpug
  stopifnot('gene' %in% colnames(data))
  
  # get pathways
  get_pathways(database, data$gene)
  
  # merge pulldown, pathways and colors
  frequencies <- assign_freq(pathways, 'pathway')
  colors <- assign_color(pathways, 'pathway')
  overlay = merge(frequencies, pulldown)
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
  
  # only significant things are plotted
  overlay = validate_reference(overlay[overlay$significant,], warn = F)

  return(list(geneset=overlay))
}





#p <- plot_volcano_basic(pulldown)
#p1 <- plot_overlay(p, as.geneset(pulldown, 'hgnc'))


