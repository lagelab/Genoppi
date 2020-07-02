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


#' @title scatter theme
#' @description Setups a centered x-y axis scatter plot with label ticks. Note,
#' that No aes will be inherited from the original ggplot. 
#' @param p the ggplot to be converted
#' @param axis_begin the x and y limits start of the axis
#' @param axis_end the x and y limits end of the axis.
#' @param total_ticks total ticks on each axis. Ideally an uneven number.
#' @export
#' @note gridlines can be entirely removed by subsequently calling \code{theme_void()}.
#' @source modified from https://stackoverflow.com/questions/17753101/center-x-and-y-axis-with-ggplot2
#' @family ggplot

scatter_theme <- function(p, axis_begin, axis_end, total_ticks = 11){
  
  # setup tick frequency and axis
  tick_frame <- data.frame(ticks = seq(axis_begin, axis_end, length.out = total_ticks), zero=0)
  tick_frame = tick_frame[tick_frame$ticks != 0, ]
  lab_frame <- data.frame(lab = seq(axis_begin, axis_end), zero = 0)
  lab_frame <- lab_frame[lab_frame$lab != 0 & lab_frame$lab %in% round(tick_frame$ticks), ]
  tick_sz <- (tail(lab_frame$lab, 1) -  lab_frame$lab[1]) / 128

  plt <- p +
    # y axis line
    geom_segment(x = 0, xend = 0, 
                 y = lab_frame$lab[1], yend = tail(lab_frame$lab, 1),
                 size = 0.5) +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = lab_frame$lab[1], xend = tail(lab_frame$lab, 1),
                 size = 0.5) +
    # x ticks
    geom_segment(data = tick_frame, 
                 aes(x = ticks, xend = ticks, 
                     y = zero, yend = zero + tick_sz), inherit.aes = F) +
    # y ticks
    geom_segment(data = tick_frame, 
                 aes(x = zero, xend = zero + tick_sz, 
                     y = ticks, yend = ticks), inherit.aes = F) + 
    
    # labels
    geom_text(data=lab_frame, aes(x=lab, y=zero, label=lab),
              family = 'Times', vjust=1.5, inherit.aes = F) +
    geom_text(data=lab_frame, aes(x=zero, y=lab, label=lab),
              family = 'Times', hjust=1.5, inherit.aes = F) +
    
    theme_minimal()
  
  return(plt)
}

