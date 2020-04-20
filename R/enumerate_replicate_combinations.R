#' @title enumerate replicate combinations
#' @description enumerates unique replicate comparisons
#' @param len number of replicates
#' @family misc
#' @export

enumerate_replicate_combinations <- function(len){
  enumerate = expand.grid(data.frame(A=1:sum(len),B=1:sum(len)))
  enumerate = as.data.frame(t(apply(enumerate,1,sort)))
  enumerate = enumerate[!duplicated(enumerate), ]
  enumerate = enumerate[enumerate$V1 != enumerate$V2,]
  colnames(enumerate) = c('repA', 'repB')
  rownames(enumerate) = NULL
  return(enumerate)
}
