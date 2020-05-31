#' @title Perform one-sample moderated t-test
#' @description Use one-sample moderated t-test implemented in limma package to calculate protein logFC, pvalue, and FDR.
#' See \code{?limma::lmFit} and \code{?limma::eBayes} for more details.
#' @param df data.frame containing gene and rep[0-9] (replicate logFC) columns.
#' @param iter integer indicating maximum number of iterations to perform in limma::limFit().
#' @return data.frame containing input df + logFC, pvalue, and FDR columns; sorted by decreasing logFC, then FDR.
#' @export
#' @examples
#' \dontrun{
#' data("example_data")
#' stats_df <- calc_mod_ttest(example_data) 
#' }
#'

calc_mod_ttest <- function(df, iter=1000){
  
  # check input
  stopifnot(is.data.frame(df))
  stopifnot(nrow(df) > 0)
  columns = grepl('rep[0-9]',colnames(df))
  
  # check for rep names
  if (all(!columns)) stop('data does not contain rep columns! LmFit failed!')
  
  # moderated t-test
  myfit <- limma::lmFit(df[,columns], method="robust", maxit=iter)
  myfit <- limma::eBayes(myfit)
  modtest <- limma::topTable(myfit, number=nrow(myfit), sort.by='none')
  colnames(modtest)[4:5] <- c("pvalue","FDR")

  # return data frame with test results: gene, rep1, rep2, ..., logFC, pvalue, FDR 
  result <- data.frame(cbind(df, modtest[,-c(2,3,6)]))
  
  # order columns
  result <- result[with(result, order(-logFC, FDR)),] 

  return(result)

} 
