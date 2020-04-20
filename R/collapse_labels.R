#' @title collaps overlay data
#' @description a data.frame used to overlay items in genoppi
#' will look for non-unqiue gene names and combine them into 
#' a single informative line using their alt_label (e.g. snp)
#' and their corrsponding dataset.
#' @param overlay a data.frame
#' @param dataset the main dataset name that is to be combined.
#' @param collapse_into the column that will contain the collapsed entries.
#' @param collapse_by what identifying colummn is the data to be collapsed by?
#' @param dataset_collapse_sep how should the dataset column and collapse-column 
#' be seperated by when collapsed?
#' @param item_sep how should each collapsed item be seperated from the next? Default is
#' newline.
#' @export
#' 

collapse_labels <- function(overlay, dataset = 'dataset', collapse_into = 'alt_label', collapse_by = 'gene', 
                            dataset_collapse_sep = ': ', item_sep = ' <br>'){
  
  # exepct parameters
  stopifnot(is.data.frame(overlay))
  stopifnot(dataset %in% colnames(overlay))
  stopifnot(collapse_by %in% colnames(overlay))
  if (collapse_into %nin% colnames(overlay)) overlay[[collapse_into]] <- ''
  
  # which rows are non-unique?
  drows = unlist(lapply(overlay[[collapse_by]], function(x) sum(overlay[[collapse_by]] == x))) > 1
  
  # combine overlapping overlays into a single lines 
  new = lapply(unique(overlay[drows, ][[collapse_by]]), function(g) {
    z = overlay[overlay[[collapse_by]] %in% g, ]
    z[[collapse_into]][is.na(z[[collapse_into]])] <- ''
    z[[collapse_into]] = paste(apply(z[,c(dataset, collapse_into)] , 1 , paste , collapse = dataset_collapse_sep), collapse = item_sep)
    return(z[1,])
  })
  
  # deal with single labels 
  old = overlay[!drows, ]
  old[[collapse_into]] = apply(old[,c(dataset, collapse_into)] , 1 , paste , collapse = dataset_collapse_sep)
  
  # conbine the filtered new overlays with alt labels with old
  combined = as.data.frame(rbind(do.call(rbind, new), old))
  
  return(combined)
}
