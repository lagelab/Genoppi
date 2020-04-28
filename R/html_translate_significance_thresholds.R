#' @title translate significance and logfc thresholds to html text
#' @description translate significance and logfc thresholds to html that
#' can be used in the shiny app for dynamic ui.
#' @param fc the logFC thresholds, i.e. a numeric value.
#' @param fc_dir the direction of the logFC: negative, positive or both.
#' @param sig_type the significance type: fdr or pvalue.
#' @param fdr_thresh the fdr fdr thresholds
#' @param pval_thresh the threshold for the p-value.
#' 
#' @export


html_translate_significance_thresholds <- function(fc, fc_dir, sig_type, fdr_thresh, pval_thresh){
  
  # track significance type and threshold
  sig_type = ifelse(sig_type == 'fdr', 'FDR', '<i>P</i>-value')
  sig_value = ifelse(sig_type == 'FDR', fdr_thresh, pval_thresh)
  fc_sign = ifelse(fc_dir, '<', '&ge;')
  region_le <- c(paste0(sig_type,"&le;", sig_value))
  region_g <- c(paste0(sig_type,">", sig_value))
  sig = list(sig=region_le, insig=region_g, sig_type=sig_type, sig_value=sig_value)
  
  # track logfc direction and threshold
  if (fc_dir == 'negative') {fc_sig = paste("log<sub>2</sub>FC&lt;", -fc); fc_insig =  paste("log<sub>2</sub>FC&ge;", -fc)}
  if (fc_dir == 'positive') {fc_sig = paste("log<sub>2</sub>FC&ge;", fc); fc_insig =  paste("log<sub>2</sub>FC&lt;", fc)}
  if (fc_dir == 'both') {fc_sig = paste("|log<sub>2</sub>FC|&ge;", fc); fc_insig = paste("|log<sub>2</sub>FC|&lt;", fc) }
  fc = list(sig=fc_sig, insig=fc_insig)
  
  return(list(sig=sig, fc=fc))
  
}
