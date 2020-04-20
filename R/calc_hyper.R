#' @title Perform hypergeometric test
#' @description Use one-tailed hypergeometric test to calcualte overlap enrichment between proteomic data and other datasets (e.g. InWeb, gene lists, etc.)
#' @param df data.frame containing proteomic data, with gene and significant columns
#' @param listDf data.frame containing data to overlap, with listName, gene and significant columns
#' @param intersectDf data.frame contaning listName and intersectN columns intersectN = vector of boolean variables indicating if total population should be intersect of the two datasets
#' @param bait name of bait protein
#' @return list of data.frame and list. Data.frame contains list_name, successInSample_count (x), sample_count (n), notSample_count (N-n), success_count (k), population_count (N), pvalue. List of lists (for each listName) contains genes names corresponding to each group (successInSample_genes, sample_genes, notSample_genes, success_genes, population_genes).
#' @examples 
#' \dontrun{
#' # an example
#' 
#' }
#' @export

calc_hyper <- function(df, listDf, intersectDf, bait=NULL){

  outDf <- NULL
  outList <- list()
  
  # check input
  if (is.null(bait)) warning('argument "bait=NULL", no bait was removed.')
  if (!is.data.frame(intersectDf)) stop('argument intersectDF but be a valid data.frame. See examples ?calc_hyper.')
  
  # intersect df is no longer required by default. Perhaps give a warning?
  #if (is.null(intersectDf)) data.frame(listName=df$listName, intersectN=T)
  #if (!is.null(intersectDf)){
  #  if (intersectDf == T) data.frame(listName=df$listName, intersectN=T)
  #  if (intersectDf == F) data.frame(listName=df$listName, intersectN=F)
  #}
  
  # perform separate enrichment test for each listName in listDf
  for (l in unique(listDf$listName)) {
    currentDf <- listDf[listDf$listName==l, ]

    # total population (N) = intersect of all genes in proteomic data  + list data
    if (intersectDf$intersectN[intersectDf$listName==l]){
      population <- unique(df$gene[df$gene %in% currentDf$gene])
    # total population (N) = all genes in proteomic data
    } else { population <- unique(df$gene) }

    # remove bait if bait is provided
    if (!is.null(bait)) { population <- population[population != bait] }

    # success in population (k) = enriched genes in proteomic data
    success <- unique(df$gene[df$significant & df$gene %in% population])
    # sample (n) = significant genes in list
    sample <- unique(currentDf$gene[currentDf$significant & currentDf$gene %in% population])
  
    # successInSample (x)
    successInSample <- intersect(success,sample)
    # population - sample (N - n)
    notSample <- setdiff(population,sample) 

    # Hypergeometric test (one-tailed)
    hyperP <- stats::phyper(length(successInSample)-1, length(sample), length(notSample), length(success), lower.tail=F)

    # add results to outDf (overlap counts and p-value) and outList (gene names coresponding to overalp counts)
    outDf <- rbind(outDf, data.frame(list_name=l, successInSample_count=length(successInSample), 
      sample_count=length(sample), notSample_count=length(notSample), success_count=length(success),
      population_count=length(population), pvalue=hyperP))
  
    outList[[l]] <- list(successInSample_genes=successInSample, sample_genes=sample,
      notSample_genes=notSample, success_genes=success, population_genes=population)

  }
 
  return(list(statistics=outDf,genes=outList))

}
