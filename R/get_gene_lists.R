#' @title Read in gene lists
#' @description Create gene list data.frame from input file
#' @param filename file path of gene list file (contain headers: listName, gene and (optional) significant columns)
#' @param sep file delimiter
#' @return list of two data.frames. First data.frame contains listName, gene and significant columns. Second data.frame contains listName and intersectN columns; intersectN = boolean variable indicating if non-significant genes are included in each list (for calling overlap enrichement functions)) 
#' @importFrom utils read.table
#' @export

get_gene_lists <- function(filename, sep="\t"){
 
  geneDf <- read.table(filename, header=T, sep)
 
  # check for columns and add accordingly
  if ("significant" %nin% names(geneDf)) { geneDf$significant <- TRUE }
  if ("listName" %nin% names(geneDf)) {geneDf$listName = 'list1' }
  
  # if geneDf contains significant=F entries
  intersectDf <- data.frame(listName=unique(geneDf$listName), 
    intersectN=sapply(unique(geneDf$listName),
    	function(x) sum(geneDf$significant[geneDf$listName==x]) < sum(geneDf$listName==x)))

  return(list(data=geneDf,intersect=intersectDf))

}
