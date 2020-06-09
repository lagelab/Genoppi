#' @title venn diagram to table
#' @description converts a venn diagram (generated from draw_genoppi_venn)
#' into a table that can be exported.
#' @param venn a list of length two that each contains a vector of genes
#' @export

venn_to_table <- function(venn){
  stopifnot(length(venn) == 2)
  venn = lapply(venn, as.character)
  vennnames = names(venn)
  
  # return individual overlaps
  ab = intersect(venn[[1]],venn[[2]])
  a = venn[[1]][venn[[1]] %nin% ab]
  b = venn[[2]][venn[[2]] %nin% ab]
  
  # convert to data.frames
  da = data.frame(gene = a, dataset = vennnames[1], stringsAsFactors = F)
  db = data.frame(gene = b, dataset = vennnames[2], stringsAsFactors = F)
  
  if (length(ab) > 0){
    dab = data.frame(gene = ab, dataset = paste(vennnames, collapse = ''), stringsAsFactors = F)
    dat = rbind(dab, da, db)
  } else {
    dat = rbind(da, db)
  }
  
  return(dat)
}
