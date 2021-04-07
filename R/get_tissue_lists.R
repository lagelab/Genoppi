#' @title Retrieve Genotype-Tissue Expression (GTEx) or
#' Human Protein Atlas specific expression data
#' @description Create gene list data.frame from input traits that can be superimposed onto volcano plots
#' or used for statistical enrichment analysis.
#' @param tissue vector of tissue names (character). See details for acceptable names.
#' @param table datasets from genoppi: hpa_rna, hpa_protein or gtex_rna.
#' @examples
#' \dontrun{
#' example_data %>% 
#'   calc_mod_ttest() %>% 
#'   id_significant_proteins() %>% 
#'   plot_volcano_basic() %>% 
#'   plot_overlay(as.bait('BCL2')) %>%
#'   plot_overlay(list(brain = get_tissue_lists("Brain_Cortex",table = gtex_rna))) %>%
#'   theme_volcano()
#' }
#' @export
#' @details
#' There are three datasets available: gtex_rna, hpa_rna and hpa_protein. See
#' \code{?hpa_rna}, \code{?gtex_rna} or \code{?gtex_protein} for more details.
#'  
#' ## The following 33 tissues are available for hpa_rna:
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
#' ## The following 53 tissue are available for gtex_rna
#' * 'Adipose_Subcutaneous'
#' * 'Adipose_Visceral_.Omentum.'
#' * 'Adrenal_Gland'
#' * 'Artery_Aorta'
#' * 'Artery_Coronary'
#' * 'Artery_Tibial'
#' * 'Bladder'
#' * 'Brain_Amygdala'
#' * 'Brain_Anterior_cingulate_cortex_.BA24.'
#' * 'Brain_Caudate_.basal_ganglia.'
#' * 'Brain_Cerebellar_Hemisphere'
#' * 'Brain_Cerebellum'
#' * 'Brain_Cortex'
#' * 'Brain_Frontal_Cortex_.BA9.'
#' * 'Brain_Hippocampus'
#' * 'Brain_Hypothalamus'
#' * 'Brain_Nucleus_accumbens_.basal_ganglia.'
#' * 'Brain_Putamen_.basal_ganglia.'
#' * 'Brain_Spinal_cord_.cervical_c.1.'
#' * 'Brain_Substantia_nigra'
#' * 'Breast_Mammary_Tissue'
#' * 'Cells_EBV.transformed_lymphocytes'
#' * 'Cells_Transformed_fibroblasts'
#' * 'Cervix_Ectocervix'
#' * 'Cervix_Endocervix'
#' * 'Colon_Sigmoid'
#' * 'Colon_Transverse'
#' * 'Esophagus_Gastroesophageal_Junction'
#' * 'Esophagus_Mucosa'
#' * 'Esophagus_Muscularis'
#' * 'Fallopian_Tube'
#' * 'Heart_Atrial_Appendage'
#' * 'Heart_Left_Ventricle'
#' * 'Kidney_Cortex'
#' * 'Liver'
#' * 'Lung'
#' * 'Minor_Salivary_Gland'
#' * 'Muscle_Skeletal'
#' * 'Nerve_Tibial'
#' * 'Ovary'
#' * 'Pancreas'
#' * 'Pituitary'
#' * 'Prostate'
#' * 'Skin_Not_Sun_Exposed_.Suprapubic.'
#' * 'Skin_Sun_Exposed_.Lower_leg.'
#' * 'Small_Intestine_Terminal_Ileum'
#' * 'Spleen'
#' * 'Stomach'
#' * 'Testis'
#' * 'Thyroid'
#' * 'Uterus'
#' * 'Vagina'
#' * 'Whole_Blood'
#' 
#' ## The following 32 tissues are avialable for hpa_protein:
#' * 'Adrenal Gland'
#' * 'Artery Aorta'
#' * 'Artery Coronary'
#' * 'Artery Tibial'
#' * 'Brain Cerebellum'
#' * 'Brain Cortex'
#' * 'Breast'
#' * 'Colon Sigmoid'
#' * 'Colon Transverse'
#' * 'GE junction'
#' * 'Esophagus Mucosa'
#' * 'Esophagus Muscle'
#' * 'Heart Atrial'
#' * 'Heart Ventricle'
#' * 'Liver'
#' * 'Lung'
#' * 'Minor Salivary'
#' * 'Muscle Skeletal'
#' * 'Nerve Tibial'
#' * 'Ovary'
#' * 'Pancreas'
#' * 'Pituitary'
#' * 'Prostate'
#' * 'Skin Unexpo'
#' * 'Skin SunExpo'
#' * 'Small Intestine'
#' * 'Spleen'
#' * 'Stomach'
#' * 'Testis'
#' * 'Thyroid'
#' * 'Uterus'
#' * 'Vagina'
#' 
#' @md


get_tissue_lists <- function(tissue, table = gtex_rna){
  
  if (!is.character(tissue)) stop('tissue input must be character')
  if (!tissue %in% table$tissue) stop(paste(tissue,'is not a valid tissue. Please see ?get_hpa_lists for details.'))
  
  return(table[table$tissue %in% tissue, ])
}

