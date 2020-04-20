#' @title Subset SNP-to-gene list
#' @description Subset a SNP-to-gene list into single-gene and multi-gene loci
#' @param df data.frame containing SNP and gene columns
#' @return list of two data.frames (singleGeneDf or multiGeneDf) corresponding to single-gene or multi-gene loci. Each contains SNP and gene columns.
#' @export

subset_snp_loci <- function(df){

  # boolean variable indicating if SNP appears more than once in df
  isMulti <- as.vector(table(df$SNP)[df$SNP]) > 1

  singleDf <- NULL
  multiDf <- NULL

  if (any(!isMulti)) {
    singleDf <- droplevels(subset(df, !isMulti))
    rownames(singleDf) <- NULL
  }
  
  if (any(isMulti)) {
    multiDf <- droplevels(subset(df, isMulti))
    rownames(multiDf) <- NULL
  }

  return(list(allGeneDf=df, singleGeneDf=singleDf, multiGeneDf=multiDf))
}
