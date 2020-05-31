#' @title Make ggplot interactive using plotly
#' @description Takes a ggplot and turns it into 
#' an interactive plot.
#' 
#' @param p a ggplot
#' @param source string. Used to connect plotly object other plotly objects.
#' @param legend boolean. Show legend for significant interactors?
#' @param sig_text what legend text should be added to a significant item?
#' 
#' @family interactivity
#' @importFrom plotly add_markers add_annotations plot_ly plotly schema
#' @export

make_interactive <- function(p, source = NULL, legend = T, sig_text = ''){
  
  # organize data and add accesion number column (temporary fix)
  overlay = p$overlay
  data = to_overlay_data(p$data) #to_overlay_data(append_to_column(p$data))
  #if (!is.null(overlay)) overlay = append_to_column(overlay, sig_text = sig_text)
  if (!is.null(overlay)) if (is.null(overlay$accession_number) & nrow(overlay) > 0) overlay$accession_number <- NA
  if (is.null(data$accession_number) & nrow(data) > 0) data$accession_number <- NA
  
  # get the global symbol and color mapping and save in local environemnt.
  sizes = c(min(c(p$data$size, p$overlay$size)), max(c(p$data$size, p$overlay$size)))
  global_colors = set_names_by_dataset(null_omit(list(data, overlay)), marker = 'color') 
  global_symbols = set_names_by_dataset(null_omit(list(data, overlay)), marker= 'symbol') 
  ggparams = p
  params = environment()

  # make plot
  p1 = plot_ly(source = source, sizes = sizes) %>%
    genoppi::add_plotly_trace(data[data$gene %nin% overlay$gene,], params)

  # add overlay
  if (!is.null(overlay)){
    if (nrow(overlay) > 0 ){
      
      p1 = p1 %>% 
        genoppi::add_plotly_trace(overlay[overlay$significant, ], params, stroke_width = 0.9, legend = legend) %>%
        genoppi::add_plotly_trace(overlay[!overlay$significant, ], params, stroke_width = 0.9, legend = F) 
      p1$overlay = p$overlay
    }
    
    # add annotations
    if (nrow(overlay) > 0 & any(overlay$label) & any(!is.na(overlay$gene)) ){
      overlay_label = overlay[overlay$label & !is.na(overlay$gene), ]
      p1 <- add_annotations(p1,
                            data = overlay_label,
                            x = ggparams$mapping$x,
                            y = ggparams$mapping$y,
                            #font = list(size = 14), make font size interactive
                            color = 'black',
                            xref = "x",
                            yref = "y",
                            text = overlay_label$gene,
                            xanchor = ifelse(overlay_label$logFC < 0, 'right', 'left'),
                            yanchor = 'bottom',
                            showarrow = F)
    }
  }
  
  p1$data <- p$data
  p1$ggparams = ggparams
  p1
}




