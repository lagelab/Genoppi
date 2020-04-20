#' @title Perform Fisher's exact test
#' @description Use one-tailed Fisher's exact test to calcualte overlap enrichment between proteomic data and other datasets (e.g. InWeb, gene lists, etc.)
#' @param df data.frame containing proteomic data, with gene and significant columns
#' @param listDf data.frame containing data to overlap, with listName, gene and significant columns
#' @param intersectDf data.frame contaning listName and intersectN columnsl intersectN = vector of boolean variables indicating if total population should be intersect of the two datasets
#' @param bait name of bait protein
#' @return list of data.frame and list. Data.frame contains list_name, overlap_count, dfOnly_count, listOnly_count, neither_count, population_count, pvalue. List of lists (for each listName) contains genes names corresponding to each group (overlap_genes, dfOnly_genes, listOnly_genes, neither_genes, population_genes).
#' @export

calc_fisher <- function(df, listDf, intersectDf, bait=NULL){

  outDf <- NULL
  outList <- list()

  # check input
  if (is.null(bait)) warning('argument "bait=NULL", no bait was removed.')
  if (!is.data.frame(intersectDf)) stop('argument intersectDF but be a valid data.frame. See examples ?calc_fisher.')
  
  # perform separate enrichment test for each listName in listDf
  for (l in unique(listDf$listName)) {
    currentDf <- listDf[listDf$listName==l, ]

    # total population = intersect of all genes in proteomic data  + list data
    if (intersectDf$intersectN[intersectDf$listName==l]){
      population <- unique(df$gene[df$gene %in% currentDf$gene])
    # total population = all genes in proteomic data
    } else { population <- unique(df$gene) }

      # remove bait if bait is provided
      if (!is.null(bait)) { population <- population[population != bait] }

      # enriched proteins/genes in proteomic data
      sigDf <- unique(df$gene[df$significant & df$gene %in% population])
      # genes in list
      sigList <- unique(currentDf$gene[currentDf$significant & currentDf$gene %in% population])

      overlap <- intersect(sigDf,sigList)
      dfOnly <- setdiff(sigDf,sigList)
      listOnly <- setdiff(sigList,sigDf)
      neither <- setdiff(setdiff(population,sigDf),sigList)

      # Fisher's exact test (one-tailed)
      fisherP <- stats::fisher.test(matrix(c(length(overlap),length(dfOnly),
        length(listOnly),length(neither)),nrow=2),alternative="greater")$p

      # add results to outDf (overlap counts and p-value) and outList (gene names coresponding to overalp counts)
      outDf <- rbind(outDf, data.frame(list_name=l, overlap_count=length(overlap), dfOnly_count=length(dfOnly),
	listOnly_count=length(listOnly), neither_count=length(neither),
        population_count=length(population), pvalue=fisherP))
      
      outList[[l]] <- list(overlap_genes=overlap, dfOnly_genes=dfOnly, listOnly_genes=listOnly,
        neither_genes=neither, population_genes=population)

  }
 
  return(list(outDf,outList))

}
