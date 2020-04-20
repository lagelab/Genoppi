#' @title search volcano plots
#' @description Adds search markers to a plot. Requires a plotly object.
#' @param p a ggplot
#' @param genes a vector of genes
#' @note internal
#' @importFrom plotly add_markers
#' @export
#' @family shiny

add_markers_search <- function(p, genes){
  
  if (is.null(p$ggparams)) stop('p$ggparams is null. Expected a ggplot!')
  p$search = p$data[grepl(genes, p$data$gene), ]
  if (nrow(p$search) > 0){
    p <- add_markers(p, data = p$search, x = p$ggparams$mapping$x, y = p$ggparams$mapping$y,
                     marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
                     textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10),
                     hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
  p
}


#' @title search pathway plots
#' @description takes a ggplot and vector as input and highligts pathways in volcano.
#' @param p a ggplot
#' @param pathways a vector of strings containing pathway names.
#' @param mapping a data.frame containing a pathway column and a gene column. Will generate
#' a mapping between genes in the plot, and their corresponding pathways found in argument
#' pathways.
#' @note internal
#' @importFrom plotly add_markers
#' @export
#' @family shiny

add_markers_search_pathway <- function(p, pathways, mapping=NULL){
  
  if (is.null(p$ggparams)) stop('p$ggparams is null. Expected a ggplot!')
  if (!any(mapping$pathway %in% pathways)) stop('arg pathways must be present in mapping')
  # c('alt_label','pathway','gene') must be present as well..

  # search main data for genes that are also in selected pathways
  data = p$data[p$data$significant, ]
  pathway_sharing_genes = mapping[mapping$pathway %in% pathways & mapping$gene %in% data$gene,]$gene
  
  # make labels bold and collapse 
  mapping[mapping$pathway %in% pathways,]$pathway <- bold(mapping[mapping$pathway %in% pathways,]$pathway)
  alt = mapping[mapping$gene %in% pathway_sharing_genes,]
  alt = collapse_labels(alt, dataset = 'pathway', collapse_into = 'alt_label', dataset_collapse_sep = '')[,c('alt_label','pathway','gene')]

  # remove duplicates
  search = merge(data, alt, by = 'gene')
  search = search[!duplicated(search$gene),]

  if (nrow(search) > 0){
    p <- add_markers(p, data = search, x = p$ggparams$mapping$x, y = p$ggparams$mapping$y,
                     marker = list(color = "#f7f4f9", size = 30, symbol = 'diamond', opacity = 0.5, line = list(width=1.5, color = "#3f007d")),
                     textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 12),
                     hovertemplate = ~paste(paste0(bold(gene), ", FDR=", signif(FDR, digits = 3),'<br>',alt_label, sep = "<br>")),
                     hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
  p
}

