#' @title Draw venn diagrams
#' @description drwas a simple 1 by 1 overlap venn diagram. 
#' @note NAs in x will be discarded and produce a warning.
#' @param x a list of two vectors that contain some overlapping strings.
#' @param colors color scheme.
#' @param main title, typically a p-value.
#' @param cat.cex numeric. the size of category labels
#' @param cat.dist the border distance between venn diagrams and labels
#' @param sub.pos numeric. the position of the labels.
#' @importFrom VennDiagram venn.diagram
#' @importFrom futile.logger flog.threshold ERROR
#' @import grid 
#' @export


draw_genoppi_venn <- function(x, main='',colors = genoppi::color_distinct(),
                              cat.cex = 1.1, cat.dist = 0.05, sub.pos = 0){
  
  # suppress logging 
  futile.logger::flog.threshold(futile.logger::ERROR)
  
  # check input
  n = length(x)
  stopifnot(n>1)
  
  # check for NAs and remove
  x = lapply(x, function(i){
    genes = as.character(i)
    count = sum(is.na(genes))
    if (count > 0) warning(paste(count,'gene(s) was not mapped and therefore removed from venn diagram.'))
    genes = as.character(na.omit(genes))
    return(genes)
  })
  
  # draw actual venn diagram
  v <- VennDiagram::venn.diagram(x,
                            col = colors[1:n], margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                            main = main, sub = " ", sub.pos = rep(sub.pos, n), scaled = F,
                            cat.cex = 1.1, cex = 2, cat.dist = rep(cat.dist, n),
                            fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
  # render the plot
  return(v)
}


