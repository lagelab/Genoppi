#' @title Retreive Human Protein Atlas specific data
#' @description Create gene list data.frame from input traits
#' @param tissue vector of tissue names (character). See details for acceptable names.
#' @param table either hpa_table or GTEX_table
#' @details
#' 
#' The following 33 tissues are available for Human Protein Atlas:
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
#' @export


get_tissue_lists <- function(tissue, table = hpa_table){
  
  if (!is.character(tissue)) stop('tissue input must be character')
  if (!tissue %in% table$tissue) stop(paste(tissue,'is not a valid tissue. Please see ?get_hpa_lists for details.'))
  
  return(table[table$tissue %in% tissue, ])
}

