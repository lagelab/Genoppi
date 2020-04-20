#' @title Identify enriched proteins
#' @description Identify enriched proteins based in input thresholds.
#' @param df data.frame contaiing logFC, pvalue, and FDR columns
#' @param logfc_dir string indicating direction of logFC cutoff ("positive" or "negative")
#' @param logfc_cutoff numeric value indicating logFC cutoff
#' @param p_cutoff numeric value indicating pvalue cutoff
#' @param fdr_cutoff numeric value indicating FDR cutoff
#' @return data.frame containing df + signficanct column of boolean values indicating which df rows (proteins) are enriched
#' @export

id_enriched_proteins <- function(df, logfc_dir="positive", logfc_cutoff=NULL, p_cutoff=NULL, fdr_cutoff=0.1){

  # input check 
  if (!is.null(logfc_cutoff) & is.null(df$logFC)) stop('logFC_cutoff specified, but logFC not in data!')
  if (!is.null(p_cutoff) & is.null(df$pvalue)) stop('p_cutoff specified, but pvalue not in data!')
  if (!is.null(fdr_cutoff) & is.null(df$FDR)) stop('fdr_cutoff specified, but FDR not in data!')
  
  sig <- rep(T,nrow(df))

  if (!is.null(logfc_dir)) {
    if (logfc_dir=="positive") sig <- sig & df$logFC > 0
    else if (logfc_dir=="negative") sig <- sig & df$logFC < 0
  }

  if (!is.null(logfc_cutoff)) {
    sig <- sig & abs(df$logFC) > abs(logfc_cutoff)
  }  

  if (!is.null(p_cutoff)) {
    sig <- sig & df$pvalue < p_cutoff
  }

  if (!is.null(fdr_cutoff)) {
    sig <- sig & df$FDR <= fdr_cutoff
  }

  df$significant <- sig
  return(df)
}
