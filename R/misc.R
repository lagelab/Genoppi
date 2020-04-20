#' @title misc tools
#' @rdname misc
#' @export

#' @description length of unique items.
#' @param x a vector or list of items.
#' @family misc
#' @export
lun <- function(x) length(unique(as.vector(x)))

#' @title not in
#' @description returns true for x not in y
#' @param x value x
#' @param y value y
#' @family misc
#' @export
'%nin%' <- function(x, y) !(x %in% y)

#' @title omit nulls from list
#' @description remove NULLs in list
#' @param lst an R list
#' @family misc
#' @export
null_omit <- function(lst) {
  lst[!vapply(lst, is.null, logical(1))]
}

#' @title warnings to stderr
#' @description sends a message to stderr (i.e shiny)
#' @param msg the message
#' @param file string, e.g. a filename.
#' @family misc
#' @export
catf <- function(msg, file = stderr()){
  if (!is.null(file)) cat(file = file, msg)
}

#' @title as.bait
#' @description quickly format the bait so that it can be used by various overlay functions.
#' @param bait string indicating the bait.
#' @family misc
#' @export
as.bait <- function(bait) return(list(bait=data.frame(gene=bait, col_significant='red', col_other='orange')))

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
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @family misc
#' @export
color_distinct <- function(){
  palette = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
  return(rep(unlist(mapply(RColorBrewer::brewer.pal, palette$maxcolors, rownames(palette))),10))
}

#' @title assign frequency
#' @description assign a frequency of occurences to a dataframe
#' @param df a data.frame
#' @param col the column which to assign frequency to.
#' @export
assign_freq <- function(df, col){
  tabl = as.data.frame(table(df[[col]]))
  colnames(tabl) <- c(col, 'Freq')
  return(merge(df, tabl, by = col))
}


#' @title bold
#' @description make text html bold
#' @param x string
#' @family misc
#' @export
bold <- function(x){paste('<b>',x,'</b>', sep='')}

#' @title italics
#' @description make text html italics 
#' @param x string
#' @family misc
#' @export
italics <- function(x){paste('<i>',x,'</i>', sep='')}
