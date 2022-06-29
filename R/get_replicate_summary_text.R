#' @title get replicate summary
#' @description a function used by shiny to generate summary tables over replicates.
#' @param thresholds a list in the format of thresholds$sig$sig and thresholds$fc$sig.
#' @param data proteomic data. Must contain a significant column.
#' @param reps a names list of replicate correlations.
#' @param digits how many digits should the table be rounded with?
#' @importFrom utils tail stack
#' @family shiny


get_replicate_summary_text <- function(thresholds, data, reps, digits = 3){
  
  cordata = stack(reps)[,c(2,1)] 
  cordata$ind = gsub('\\.', ' vs ', cordata$ind)
  cordata = rbind(cordata, c('average', mean(cordata$values)))
  cordata$values <- as.numeric(cordata$values)
  cordata$values = round(cordata$values, digits = digits)
  colnames(cordata) = c('Comparison', 'Correlation (r)')
  text = paste('Enrichment threshold:', thresholds$sig$sig, 'and', thresholds$fc$sig, '<br>',
               bold(sum(data$significant)), 'out of', bold(sum(nrow(data))), 'proteins enriched.')
  
  return(list(outtext = text, outtable = cordata))
  
}

