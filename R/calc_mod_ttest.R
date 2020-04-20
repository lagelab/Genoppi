#' @title Perform moderated t-test
#' @description Use moderated t-test implemented in limma package to calculate logFC, pvalue, and FDR.
#' See \code{?limma::lmFit} and \code{?limma::eBayes} for more details.
#' @param df data.frame/data.table containing gene and rep (replicate logFC)  columns
#' @return data.frame containing containing df + logFC, pvalue, and FDR; sorted by decreasing logFC
#' @export

calc_mod_ttest <- function(df){
  
  # check input
  stopifnot(data.table::is.data.table(df))
  stopifnot(nrow(df) > 0)
  columns = grepl('rep[0-9]',colnames(df))
  
  # moderated t-test
  myfit <- limma::lmFit(df[,columns, with = F], method="robust") #myfit <- limma::lmFit(subset(df, select=-c(gene)), method="robust")
  myfit <- limma::eBayes(myfit)
  modtest <- limma::topTable(myfit, number=nrow(myfit), sort.by='none')
  colnames(modtest)[4:5] <- c("pvalue","FDR")

  # return data frame with test results: gene, rep1, rep2, ..., logFC, pvalue, FDR 
  result <- data.frame(cbind(df, modtest[,-c(2,3,6)]))
  result <- result[with(result, order(-logFC, FDR)),]
  
  # order columns

  return(result)
} 
