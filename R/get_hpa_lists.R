#' @title Retreive Human Protein Atlas specific data
#' @description Create gene list data.frame from input traits
#' @param tissue vector of tissue names (character). See details for acceptable names.
#' @param genes vector of gene names (genes detected in proteomic data)
#' @details
#' 
#' The following 33 tissues are available:
#' 
#' * 'adipose tissue'
#' * 'adrenal gland'
#' * 'bone marrow'
#' * 'breast'
#' * 'cervix, uterine'
#' * 'ductus deferens'
#' * 'endometrium 1'
#' * 'epididymis'
#' * 'esophagus'
#' * 'fallopian tube'
#' * 'gallbladder'
#' * 'heart muscle'
#' * 'kidney'
#' * 'liver'
#' * 'lung'
#' * 'ovary'
#' * 'pancreas'
#' * 'parathyroid gland'
#' * 'pituitary gland'
#' * 'placenta'
#' * 'prostate'
#' * 'retina'
#' * 'salivary gland'
#' * 'seminal vesicle'
#' * 'skeletal muscle'
#' * 'skin 1'
#' * 'smooth muscle'
#' * 'stomach 1'
#' * 'testis'
#' * 'thyroid gland'
#' * 'tongue'
#' * 'urinary bladder'
#' * 'vagina'
#' 
#' @md
#' @examples 
#' \dontrun{
#' 
#' 
#' }
#' @export


get_hpa_lists <- function(tissue){
  
  if (!is.character(tissue)) stop('tissue input must be character')
  if (!tissue %in% hpa_table$tissue) stop(paste(tissue,'is not a valid tissue. Please see ?get_hpa_lists for details.'))
  
  return(hpa_table[hpa_table$tissue %in% tissue, ])
}

