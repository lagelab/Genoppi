#' @title Draw venn diagrams
#' @description drwas a simple 1 by 1 overlap venn diagram. 
#' @note NAs in x will be discarded and produce a warning.
#' @param x a list of two vectors that contain some overlapping strings.
#' @param colors color scheme.
#' @param main title, typically a p-value.
#' @importFrom VennDiagram venn.diagram
#' @importFrom futile.logger flog.threshold ERROR
#' @import grid 
#' @export


draw_genoppi_venn <- function(x, main='',colors = c("cornflowerblue", "yellow1")){
  
  # check input
  stopifnot(length(x) == 2)
  stopifnot(length(colors) == 2)
  
  # check for NAs
  x = lapply(x, function(i){
    genes = as.character(i)
    count = sum(is.na(genes))
    if (count > 0) warning(paste(count,'gene(s) was not mapped and therefore removed from venn diagram.'))
    genes = as.character(na.omit(genes))
    return(genes)
  })
  
  # draw actual venn diagram
  v <- VennDiagram::venn.diagram(x,
                            col = colors, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                            main = main,
                            sub = " ", sub.pos = c(0, 0), scaled = F,
                            cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05),
                            fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
  # render the plot
  return(v)
}
