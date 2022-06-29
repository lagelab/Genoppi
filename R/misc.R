#' @title length of unique 
#' @description get the length of unique items.
#' @param x a vector or list of items.
#' @family misc
lun <- function(x) length(unique(as.vector(x)))

#' @title not in
#' @description returns true for x not in y
#' @param x value x
#' @param y value y
#' @family misc
'%nin%' <- function(x, y) !(x %in% y)

#' @title omit nulls from list
#' @description remove NULLs in list
#' @param lst an R list
#' @family misc
null_omit <- function(lst) {
  lst[!vapply(lst, is.null, logical(1))]
}

#' @title warnings to stderr
#' @description sends a message to stderr (i.e shiny)
#' @param msg the message
#' @param file string, e.g. a filename.
#' @family misc
catf <- function(msg, file = stderr()){
  if (!is.null(file)) cat(file = file, msg)
}

#' @title as.bait
#' @description quickly format a gene as a bait so that it can be used by various overlay functions.
#' This functional will create a named data.frame with column 'gene', 'col_significant' and
#' 'col_other', that can be directly inputted into plot_overlay.
#' @param bait string indicating the bait.
#' @family misc
#' @examples 
#' \dontrun{
#' # overlay the bait
#' example_data %>%
#'   calc_mod_ttest() %>%
#'   id_significant_proteins() %>%
#'   plot_volcano_basic() %>%
#'   plot_overlay(as.bait('BCL2')) %>%
#' }
#' @export
as.bait <- function(bait) return(list(bait=data.frame(gene=bait, col_significant='red', col_other='orange', dataset = 'bait')))


#' @title as gene of interest
#' @description quickly format the gene so that it can be used by various overlay functions.
#' This functional will create a named data.frame with column 'gene', 'col_significant' and
#' 'col_other', that can be directly inputted into plot_overlay.
#' @param genes string indicating the bait.
#' @param col_significant color of significant interactor.
#' @param col_other color of non-significant interactor.
#' @param shape numeric. 21 is default for circle.
#' @param dataset used internally in genoppi to plot multiple datasets.
#' @family misc
#' @examples 
#' \dontrun{
#' # overlay the bait
#' example_data %>%
#'  calc_mod_ttest() %>%
#'  id_significant_proteins() %>%
#'  plot_volcano_basic() %>%
#'  plot_overlay(as.goi(c('BCL2', 'FUS', 'TRIM28')))
#' }
#' @export
as.goi <- function(genes, col_significant = 'cyan', col_other = 'grey', shape = 21, dataset = 'GOI') {
  df = data.frame(gene=genes, col_significant=col_significant, col_other=col_other, shape=shape, dataset=dataset)
  return(list(goi=df))
}


#' @title split string by character numbers
#' @description split a string by a character
#' @param x a vector of strings.
#' @param nchar integer.
#' @param suffix  string. what should all strings end with?
strsplit.nchar <- function(x, nchar, suffix = '...'){
  return(lapply(strsplit(x, ''), function(x) paste0(paste(x[1:nchar], collapse = ''), suffix)))
}

#' @title color gradient
#' @description makes a function for getting gradient colors
#' @param x vector of values
#' @param colors colors from which to form gradient.
#' @param colsteps how many colors should be created?
#' @family misc
#' @importFrom grDevices colorRampPalette
#' @export
#' @source stackoverflow snippet
color_gradient <- function(x, colors=c("green", 'red'), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

#' @title distinct coloring
#' @description generates vector of 74 distinct colors from RColorBrewer.
#' @param length.out repeats the vector.
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @family misc
#' @export
color_distinct <- function(length.out=74){
  palette = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
  return(rep_len(unlist(mapply(RColorBrewer::brewer.pal, palette$maxcolors, rownames(palette))), length.out))
}

#' @title assign frequency
#' @description assign a frequency of occurences to a dataframe
#' @param df a data.frame
#' @param col the column which to assign frequency to.
#' 
assign_freq <- function(df, col){
  tabl = as.data.frame(table(df[[col]]))
  colnames(tabl) <- c(col, 'Freq')
  return(merge(df, tabl, by = col))
}

#' @title assign color by column
#' @description assigns color to a data.frame by a certain column
#' @param df a data.frame
#' @param col the identifiying column for assiging color
#' @param order_by_freq boolean. Should the data be order by frequency of col?
#' 
assign_color <- function(df, col, order_by_freq = T){
  
  tabl = data.frame(table(df[[col]]), color = NA)
  colnames(tabl) <- c(col, 'Freq', 'color')
  n = nrow(tabl)
  tabl$color = color_distinct(n) #rep(palette, 10)[1:(min(length(palette), n))]
  
  # warnings and checks
  if (order_by_freq) tabl = tabl[rev(order(tabl$Freq)),]
  #if (n > length(palette)) warning('There were more unique entries than the color palette. Re-using palette!')
  tabl$Freq <- NULL
  
  return(merge(df, tabl, by = col))
}


#' @title bold
#' @description make text html bold
#' @param x string
#' @family misc
bold <- function(x){paste('<b>',x,'</b>', sep='')}

#' @title italics
#' @description make text html italics 
#' @param x string
#' @family misc
#' 
italics <- function(x){paste('<i>',x,'</i>', sep='')}

#' @title get table of plotly/ggplot symbols
#' @description get a table for translating
#' between plotly symbols and ggplot shapes.
#' This is primiarly used in the shiny application
#' to ensure consistency between ggplot and plotly.
#' @family misc
table_symbols <- function() {
  d = data.frame(
    shape=c(0:12,21:25),
             symbol=c('square-open', # 0
                      'circle-open', # 1
                      'triangle-up-open', # 2
                      'cross-open', # 3
                      'x-open', # 4
                      'diamond-open', # 5
                      'triangle-down-open', # 6
                      'square-x-open', # 7
                      'asterisk-open', # 8
                      'diamond-cross-open',
                      'circle-cross-open', # 10
                      'star-diamond-open', # 11
                      'square-cross-open', # 12
                      #'circle', # 13
                      #'square-x-open', # 14
                      #'square', # 15
                      #'circle', # 16
                      #'triangle', # 17
                      #'diamond', # 18
                      #'circle', # 19
                      #'circle', # 20
                      'circle', # 21 
                      'square', # 22
                      'diamond', # 23
                      'triangle-up',
                      'triangle-down'), # 25
             stringsAsFactors = F)

  # open/closed symbols
  d$open <- grepl('open', d$symbol)
  return(d[order(d$symbol),])
}

#' @title translate ggplot shapes to plotly symbols
#' @param vec a character vector of ggplot shapes.
#' @family misc
#' @note see ?table_symbols for allowed shapes and symbols.
#' 
shape_to_symbol <- function(vec){
  tabl = table_symbols()
  stopifnot(is.numeric(vec))
  stopifnot(all(vec %in% tabl$shape))
  res = unlist(lapply(vec, function(x){tabl$symbol[tabl$shape == x]}))
  return(res)
}

#' @title translate plotly symbols to ggplot shapes
#' @param vec a character vector of plotly symbols.
#' @family misc
#' 
symbol_to_shape <- function(vec){
  tabl = table_symbols()
  stopifnot(all(vec %in% tabl$symbol))
  res = unlist(lapply(vec, function(x){tabl$shape[tabl$symbol == x][1]}))
  return(res)
}
  
#' @title get ggplot legend
#' @description gets a ggplot legend
#' @param a.gplot a ggplot
#' @note modified from \url{https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2}
#' @family misc
#' @importFrom ggplot2 ggplot_gtable
#' 
get_gg_legend <- function(a.gplot){ 
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

#' @title is columns
#' @description conducts a test on a specific
#' subset of columns in the data.frame
#' @param df the data.frame
#' @param col string. Column name regex.
#' @param test a function. E.g. is.numeric
is_cols <- function(df, col, test){
  cnames = colnames(df)
  if (!any(grepl(col, cnames))) stop(paste(col,'was not found!'))
  return(all(unlist(lapply(df[,grepl(col, cnames)], test))))
}
  

#' @title vertical line
#' @description vertical plotly line
#' @param x x-value, numeric.
#' @param color color, string.
#' @param width line width, numeric.
#' @param dash string. 'solid, 'dot' or 'dash'.
#' @family misc
vline <- function(x = 0, color = "red", width = 1, dash = 'dash') {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, width = width, dash = dash)
  )
}

#' @title horizontal line
#' @description horizontal plotly line
#' @param y y-value, numeric.
#' @param color color, string.
#' @param width line width, numeric.
#' @param dash string. 'solid', 'dot' or 'dash'.
#' @family misc
hline <- function(y = 0, color = "blue", width = 1, dash = 'dash') {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, width = width, dash = dash)
  )
}
  

#' @title hyperlink
#' @description convert a string into a hyperlink
#' @param url what is the url?
#' @param text what should be displayed? Default is NULL,
#' which just displays the URL.
#' @family html

hyperlink <- function(url, text){
  return(paste0("<a href='",mydata$url,"'>",mydata$url,"</a>"))
}

#' @title line_unity
#' @description plots a unity line

line_unity <- function(){geom_abline(intercept=0, slope=1, linetype="longdash", size=0.2)}


#' @title find docs
#' @description find documentation objects for shiny app.
#' @param dir directory
#' @param file file / regex pattern

find_docs <- function(file = 'inweb_table.info', dir = 'documentation'){
  return(list.files(dir, pattern = file, full.names = T))
}



