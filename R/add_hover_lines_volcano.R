#' @title add hover lines
#' @description A function that adds reactive dashed lines to a plotly object.
#' @param p a ggplot
#' @param line_pvalue the pvalue threshold
#' @param line_logfc the logfc threshold 
#' @param logfc_direction string. Can be either \code{'positive'}, \code{'negative'} or \code{'both'}.
#' @param sig_type string. Can be either \code{'fdr'} or \code{'pvalue'}.
#' @importFrom plotly add_lines
#' @family shiny
#' @export
add_hover_lines_volcano <- function(p, line_pvalue, line_logfc, logfc_direction = 'both', sig_type='fdr'){
  
  stopifnot(!is.null(p$data))
  
    # p-value line (horizontal lines)
    if (sig_type %in% 'pvalue'){
      p <- p %>% add_lines(x = ~c(min(p$data$logFC)-0.5, max(p$data$logFC)+0.5), y = ~-log10(line_pvalue), line = list(dash = "dash", width = 0.5, color = "#2b333e"),
                           name = '', hoverinfo = "text", text = paste0("pvalue = ", line_pvalue), showlegend = F)
    }
  
    # logfc lines (vertical lines)
    if (logfc_direction %in%  c('both', 'negative')){
      p <-  p %>% add_lines(x = -line_logfc, y = ~c(min(-log10(p$data$pvalue)-0.5), max(-log10(p$data$pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
                  name = '', hoverinfo = "text", text = paste0("logFC = ", -line_logfc), showlegend = F) 
    }
    if (logfc_direction %in% c('both', 'positive')){
      p <- p %>% add_lines(x = line_logfc, y = ~c(min(-log10(p$data$pvalue)-0.5), max(-log10(p$data$pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
                name = '', hoverinfo = "text", text = paste0("logFC = ", line_logfc), showlegend = F)
    }
  

  p
}
