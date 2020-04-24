#' @title Perform moderated t-test
#' @description Use moderated t-test implemented in limma package to calculate logFC, pvalue, and FDR.
#' See \code{?limma::lmFit} and \code{?limma::eBayes} for more details.
#' @param df data.frame containing gene and rep (replicate logFC)  columns
#' @return data.frame containing containing df + logFC, pvalue, and FDR columns; sorted by decreasing logFC, then FDR
#' @export
#' @examples
#' /dontrun{
#' data("example_data")
#' stats_df <- calc_mod_ttest(example_data) 
#' }
#'

calc_mod_ttest <- function(df){
  
  # check input
  stopifnot(is.data.frame(df))
  stopifnot(nrow(df) > 0)
  
  columns = grepl('rep[0-9]',colnames(df))
  
  # moderated t-test
  myfit <- limma::lmFit(df[,columns], method="robust")
  myfit <- limma::eBayes(myfit)
  modtest <- limma::topTable(myfit, number=nrow(myfit), sort.by='none')
  colnames(modtest)[4:5] <- c("pvalue","FDR")

  # return data frame with test results: gene, rep1, rep2, ..., logFC, pvalue, FDR 
  result <- data.frame(cbind(df, modtest[,-c(2,3,6)]))
  
  # order columns
  result <- result[with(result, order(-logFC, FDR)),] 

  return(result)

} 
