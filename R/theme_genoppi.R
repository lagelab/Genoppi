#' Genoppi GG theme
#' @description Set's up the classic genoppi theme for ggplot2.
#' @author April/Frederik
#' @importFrom ggplot2 element_text theme element_blank
#' @export

theme_genoppi <- function(){
  p <- theme_bw() + 
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"),
          aspect.ratio=1)
  p
}

#' @title genoppi themed bar
#' @param rotate boolean. Rotates axis titles.
#' @description genoppi themed bar for shiny
theme_genoppi_bar <- function(rotate = F){
  if (rotate == FALSE){
    p <- theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               panel.background=element_blank(),
               plot.title = element_text(size = rel(1)))
  } else {
    p <- theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               panel.background=element_blank(),
               plot.title = element_text(size = rel(1)))
  }
  p
}
