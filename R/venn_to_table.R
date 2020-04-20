#' @title venn diagram to table
#' @description converts a venn diagram (generated from draw_genoppi_venn)
#' into a table that can be exported.
#' @param venn a list of length two that each contains a vector of genes
#' @param datnames a vector of string with the names of the venn items
#' @export

venn_to_table <- function(venn){
  stopifnot(length(venn) == 2)
  venn = lapply(venn, as.character)
  intersect = intersect(venn[[1]],venn[[2]])
  a = venn[[1]][venn[[1]] %nin% intersect]
  b = venn[[2]][venn[[2]] %nin% intersect]
  dat = data.frame(
    A = c(a, intersect, rep(NA, length(b))),
    B = c(rep(NA, length(a)), intersect, b),
    stringsAsFactors = F
  )
  dat$overlap = 0
  bool = !is.na(dat$A) & !is.na(dat$B)
  if (any(bool)) dat[bool,]$overlap = 1
  return(dat)
}
