#' @title Genoppi GG theme
#' @description Set's up the classic genoppi theme for ggplot2.
#' @importFrom ggplot2 element_text theme element_blank
#' @family ggplot
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
#' @family ggplot
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


#' @title scatter theme old
#' @description Setups a centered x-y axis scatter plot with label ticks. Note,
#' that No aes will be inherited from the original ggplot. Also, he user will 
#' have to adjust the limits of x and y axis.
#' @param p the ggplot to be converted
#' @param axis_begin the x and y limits start of the axis
#' @param axis_end the x and y limits end of the axis.
#' @param total_ticks total ticks on each axis. Ideally an uneven number.
#' @param grid_width the width of the grid.
#' @note gridlines can be entirely removed by subsequently calling \code{theme_void()}.
#' @source modified from https://stackoverflow.com/questions/17753101/center-x-and-y-axis-with-ggplot2
#' @family ggplot
#' @export

scatter_theme_old <- function(p, axis_begin, axis_end, total_ticks = 11, grid_width = 1){
  
  warning('depreacted. Will be removed in future release.')
  
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
                 size = 0.5, inherit.aes = F) +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = lab_frame$lab[1], xend = tail(lab_frame$lab, 1),
                 size = 0.5, inherit.aes = F) +
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
    
    # setup grid and axis
    theme_minimal() +  
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_line(size = (0.25)),
          panel.grid.minor = element_line(size = (0.25))) +
    scale_y_continuous(minor_breaks = seq(axis_begin, axis_end, grid_width), 
                       breaks = seq(axis_begin, axis_end, grid_width*2)) +
    scale_x_continuous(minor_breaks = seq(axis_begin, axis_end, grid_width), 
                       breaks = seq(axis_begin, axis_end, grid_width*2))
  
  return(plt)
}


#' @title volcano theme
#' @description Setups a scatter cross theme using limits from ggplot. The user will have to manually specify aesthetics, such
#' as axis widths, tick sizes etc.
#' @param p the ggplot to be converted
#' @param tick_sz_x Tick-size. 
#' @param tick_sz_y tick size. 
#' @param normalize_tick_sz Boolean. If TRUE, the tick_sz_y will be normalized according to argument tick_sz_x.
#' @param axes_width the width of the x and y-axis.
#' @param ticks_width the width of the ticks.
#' @param ticks_labs_cex the character expansion factor of tick labels.
#' @param ticks_labs_x_vjust Vertical adjustment of tick labels on x axis.
#' @param ticks_labs_y_hjust Horizontal adjustment of tick labels on y axis.
#' @param xlab_vjust Vertical adjustment on label on x-axis.
#' @param xlab_hjust Horizontal adjustment on label on x-axis.
#' @param ylab_vjust Vertical adjustment on label on y-axis.
#' @param ylab_hjust Horizontal adjustment on label on y-axis.
#' @param font_family string for the font family.
#' @param grid_lines Boolean. Draw grid lines?
#' @param xlab String. alternative x label.
#' @param ylab String. alternative y label.
#' @param lab_cex numeric. Character expansion factor for labels.
#' 
#' 
#' @family ggplot
#' @export

volcano_theme <- function(p, tick_sz_x = -0.15, tick_sz_y = NULL, normalize_tick_sz = T,
                          axes_width = 0.50, ticks_width = 0.70, ticks_labs_cex = 4, 
                          ticks_labs_x_vjust = 2, ticks_labs_y_hjust = 2, lab_cex = 2,
                          xlab_vjust = 0.60, xlab_hjust = 0.38, ylab_vjust = 1.5, ylab_hjust = 0,
                          font_family = 'Helvetica', grid_lines = F, xlab = NULL, ylab = NULL){
  
  # get axis limits
  ggbuild <- ggplot_build(p)
  xlims <- ggbuild$layout$panel_params[[1]]$x.range
  ylims <- ggbuild$layout$panel_params[[1]]$y.range
  ylims[1] <- 0
  
  # get breaks and labels for x-axis
  tick_frame_x <- data.frame(ticks = ggbuild$layout$panel_params[[1]]$x$minor_breaks, zero = 0)
  #tick_frame_x <- tick_frame_x[tick_frame_x$ticks != 0, ]
  lab_frame_x <- data.frame(lab = na.omit(ggbuild$layout$panel_params[[1]]$x$breaks), zero = 0)
  #lab_frame_x <- lab_frame_x[lab_frame_x$lab != 0 & lab_frame_x$lab %in% round(tick_frame_x$ticks), ]
  
  # get breaks and labels for y-axis
  tick_frame_y <- data.frame(ticks = ggbuild$layout$panel_params[[1]]$y$minor_breaks, zero = 0)
  tick_frame_y <- tick_frame_y[tick_frame_y$ticks != 0, ]
  lab_frame_y <- data.frame(lab = na.omit(ggbuild$layout$panel_params[[1]]$y$breaks), zero = 0)
  lab_frame_y <- lab_frame_y[lab_frame_y$lab != 0 & lab_frame_y$lab %in% round(tick_frame_y$ticks), ]
  
  # ticks for x and y axis
  #tick_frame_x <-  data.frame(ticks = seq(xlims[1], xlims[2], length.out = x_ticks), zero=0)
  #tick_frame_x <- tick_frame_x[tick_frame_x$ticks != 0, ]
  #tick_frame_y <- data.frame(ticks = seq(ylims[1], ylims[2], length.out = y_ticks), zero=0)
  #tick_frame_y <- tick_frame_y[tick_frame_y$ticks >= 0, ]

  # labels for x and y axis
  #lab_frame_x <- data.frame(lab = seq(xlims[1], xlims[2]), zero = 0)
  #lab_frame_x <- lab_frame_x[lab_frame_x$lab != 0 & lab_frame_x$lab %in% round(tick_frame_x$ticks), ]
  #lab_frame_y <- data.frame(lab = seq(ylims[1], ylims[2]), zero = 0)
  #lab_frame_y <- lab_frame_y[lab_frame_y$lab != 0 & lab_frame_y$lab %in% round(tick_frame_y$ticks), ]
  tick_sz_x <- ifelse(is.null(tick_sz_x), (tail(lab_frame_x$lab, 1) -  lab_frame_x$lab[1]) / 128, tick_sz_x)
  tick_sz_y <- ifelse(is.null(tick_sz_y), (tail(lab_frame_y$lab, 1) -  lab_frame_y$lab[1]) / 128, tick_sz_y)
  
  # normalize tick size to match different axes sizes
  if (normalize_tick_sz){
    a = (ylims[2]^2)/(xlims[2]^2)
    tick_sz_y = a * tick_sz_x 
  }
  
  
  plt <- p +
    # y axis line
    geom_segment(x = 0, xend = 0, 
                 y = 0, yend = ylims[2],
                 size = axes_width, inherit.aes = F) +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = xlims[1], xend = xlims[2],
                 size = axes_width, inherit.aes = F) +
    # x ticks
    geom_segment(data = tick_frame_x, 
                 aes(x = ticks, xend = ticks, 
                     y = zero, yend = zero + tick_sz_y), 
                 size = ticks_width,
                 inherit.aes = F) +
    # y ticks
    geom_segment(data = tick_frame_y, 
                 aes(x = zero, xend = zero + tick_sz_x, 
                     y = ticks, yend = ticks), 
                 size = ticks_width,
                 inherit.aes = F) + 
    
    # labels
    geom_text(data=lab_frame_x, aes(x=lab, y=zero, label=lab),
              family = font_family, vjust = ticks_labs_x_vjust, size = ticks_labs_cex, inherit.aes = F) +
    geom_text(data=lab_frame_y, aes(x=zero, y=lab, label=lab),
              family = font_family, hjust = ticks_labs_y_hjust, size = ticks_labs_cex, inherit.aes = F) +
    
    # axis labels
    annotate('text', x = 0.5, y = tail(tick_frame_y$ticks, 1)-0.5, 
            label =  ifelse(is.null(ylab), deparse(p$labels$y), ylab), angle = 90, parse = T, 
            vjust= ylab_vjust, hjust = ylab_hjust,
            family = font_family,
            size = lab_cex) +
    
    annotate('text', x = 0, y = -0.3, 
             label =  ifelse(is.null(xlab) ,deparse(p$labels$x), xlab), parse = T, 
             vjust = xlab_vjust, hjust = xlab_hjust,
             family = font_family,
             size = lab_cex)
  
    # setup grid and axis
    plt = plt + theme_minimal() +  
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y = element_blank())
    
    if (grid_lines == F){
      plt = plt + theme(panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank())  
        
    }    
    #panel.grid.major = element_line(size = (0.25)),
    #panel.grid.minor = element_line(size = (0.25))) +
    #scale_y_continuous(minor_breaks = seq(ylims[1], ylims[2], grid_width/2), breaks = seq(ylims[1], ylims[2], grid_width)) +
    #$scale_x_continuous(minor_breaks = seq(xlims[1], xlims[2], grid_width), breaks = seq(xlims[1], xlims[2], grid_width*2))
  
  return(plt)
}



#' @title volcano theme
#' @description Setups a volcano theme using limits from ggplot. The user will have to manually specify aesthetics, such
#' as axis widths, tick sizes etc.
#' @param p the ggplot to be converted
#' @param tick_sz_x Tick-size. 
#' @param tick_sz_y tick size. 
#' @param normalize_tick_sz Boolean. If TRUE, the tick_sz_y will be normalized according to argument tick_sz_x.
#' @param axes_width the width of the x and y-axis.
#' @param ticks_width the width of the ticks.
#' @param ticks_labs_cex the character expansion factor of tick labels.
#' @param ticks_labs_x_vjust Vertical adjustment of tick labels on x axis.
#' @param ticks_labs_y_hjust Horizontal adjustment of tick labels on y axis.
#' @param xlab_vjust Vertical adjustment on label on x-axis.
#' @param xlab_hjust Horizontal adjustment on label on x-axis.
#' @param ylab_vjust Vertical adjustment on label on y-axis.
#' @param ylab_hjust Horizontal adjustment on label on y-axis.
#' @param font_family string for the font family.
#' @param grid_lines Boolean. Draw grid lines?
#' 
#' @note gridlines can be entirely removed by subsequently calling \code{theme_void()}.
#' 
#' @source modified from https://stackoverflow.com/questions/17753101/center-x-and-y-axis-with-ggplot2
#' @family ggplot
#' @export


scatter_theme <- function(p, tick_sz_x = -0.15, tick_sz_y = NULL, normalize_tick_sz = T,
                          axes_width = 0.50, ticks_width = 0.70, ticks_labs_cex = 4, 
                          ticks_labs_x_vjust = 2, ticks_labs_y_hjust = 2,
                          xlab_vjust = 0.60, xlab_hjust = 0.38, ylab_vjust = 1.5, ylab_hjust = 0,
                          font_family = 'Helvetica', grid_lines = F){
  
  # get axis limits
  ggbuild <- ggplot_build(p)
  xlims <- ggbuild$layout$panel_params[[1]]$x.range
  ylims <- ggbuild$layout$panel_params[[1]]$y.range
  
  # get breaks and labels for x-axis
  tick_frame_x <- data.frame(ticks = ggbuild$layout$panel_params[[1]]$x$minor_breaks, zero = 0)
  tick_frame_x <- tick_frame_x[tick_frame_x$ticks != 0, ]
  lab_frame_x <- data.frame(lab = na.omit(ggbuild$layout$panel_params[[1]]$x$breaks), zero = 0)
  lab_frame_x <- lab_frame_x[lab_frame_x$lab != 0 & lab_frame_x$lab %in% round(tick_frame_x$ticks), ]
  
  # get breaks and labels for y-axis
  tick_frame_y <- data.frame(ticks = ggbuild$layout$panel_params[[1]]$y$minor_breaks, zero = 0)
  tick_frame_y <- tick_frame_y[tick_frame_y$ticks != 0, ]
  lab_frame_y <- data.frame(lab = na.omit(ggbuild$layout$panel_params[[1]]$y$breaks), zero = 0)
  lab_frame_y <- lab_frame_y[lab_frame_y$lab != 0 & lab_frame_y$lab %in% round(tick_frame_y$ticks), ]
  
  # tick size 
  tick_sz_x <- ifelse(is.null(tick_sz_x), (tail(lab_frame_x$lab, 1) -  lab_frame_x$lab[1]) / 128, tick_sz_x)
  tick_sz_y <- ifelse(is.null(tick_sz_y), (tail(lab_frame_y$lab, 1) -  lab_frame_y$lab[1]) / 128, tick_sz_y)
  
  # normalize tick size to match different axes sizes
  if (normalize_tick_sz){
    a = (ylims[2]^2)/(xlims[2]^2)
    tick_sz_y = a * tick_sz_x 
  }
  
  
  plt <- p +
    # y axis line
    geom_segment(x = 0, xend = 0, 
                 y = ylims[1], yend = ylims[2],
                 size = axes_width, inherit.aes = F) +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = xlims[1], xend = xlims[2],
                 size = axes_width, inherit.aes = F) +
    # x ticks
    geom_segment(data = tick_frame_x, 
                 aes(x = ticks, xend = ticks, 
                     y = zero, yend = zero + tick_sz_y), 
                 size = ticks_width,
                 inherit.aes = F) +
    # y ticks
    geom_segment(data = tick_frame_y, 
                 aes(x = zero, xend = zero + tick_sz_x, 
                     y = ticks, yend = ticks), 
                 size = ticks_width,
                 inherit.aes = F) + 
    
    # labels
    geom_text(data=lab_frame_x, aes(x=lab, y=zero, label=lab),
              family = font_family, vjust = ticks_labs_x_vjust, size = ticks_labs_cex, inherit.aes = F) +
    geom_text(data=lab_frame_y, aes(x=zero, y=lab, label=lab),
              family = font_family, hjust = ticks_labs_y_hjust, size = ticks_labs_cex, inherit.aes = F)
  
  # setup grid and axis
  plt = plt + theme_minimal() +  
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          #axis.title.x = element_blank(),
          axis.ticks.y=element_blank())
          #axis.title.y = element_blank())
  
  if (grid_lines == F){
    plt = plt + theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank())  
    
  }    
  
  #panel.grid.major = element_line(size = (0.25)),
  #panel.grid.minor = element_line(size = (0.25))) +
  #scale_y_continuous(minor_breaks = seq(ylims[1], ylims[2], grid_width/2), breaks = seq(ylims[1], ylims[2], grid_width)) +
  #$scale_x_continuous(minor_breaks = seq(xlims[1], xlims[2], grid_width), breaks = seq(xlims[1], xlims[2], grid_width*2))
  
  return(plt)
}




