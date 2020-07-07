#' @title Draw venn diagrams
#' @description drwas a simple 1 by 1 overlap venn diagram. 
#' @note NAs in x will be discarded and produce a warning.
#' @param x a list of two vectors that contain some overlapping strings.
#' @param colors color scheme.
#' @param main title, typically a p-value.
#' @param cat.cex numeric. the size of category labels
#' @param cat.dist the border distance between venn diagrams and labels
#' @param sub.pos numeric. the position of the labels.
#' @param alpha numeric, transparency value between 0 and 1.
#' @param fill the fill color of the venn diagrams.
#' @param margin margin around the plot.
#' @importFrom VennDiagram venn.diagram
#' @importFrom futile.logger flog.threshold ERROR
#' @import grid 
#' @export


draw_genoppi_venn <- function(x, main='',colors = color_distinct(), alpha = NULL,
                              cat.cex = 1.1, cat.dist = 0.05, sub.pos = 0, fill = NULL,
                              margin = 0.05){
  
  # suppress logging 
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  
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
                            col = colors[1:n], fill = fill, alpha = alpha,
                            margin=margin, filename = NULL, resolution = 900, height = 400, force.unique = T,
                            main = main, sub = " ", sub.pos = rep(sub.pos, n), scaled = F,
                            cat.cex = 1.1, cex = 2, cat.dist = rep(cat.dist, n),
                            fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
  
  # render the plot
  return(v)

}


#' @title plot venn diagram 
#' @description plots a venn at a reduced scale to allow for longer
#' diagram names.
#' @param v a venn diagram made from package 'VennDiagram' or from genoppi::draw_genoppi_venn().
#' @param scale what scale should it be plotted at?
#' @export
plot_venn <- function(v, scale = 0.9){
  grid.newpage()
  pushViewport(viewport(width=unit(scale, "npc"), height = unit(scale, "npc")))
  grid.draw(v)
}




