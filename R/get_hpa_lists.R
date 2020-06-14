#' @title Retreive Human Protein Atlas specific data
#' @description Create gene list data.frame from input traits
#' @param tissue vector of tissue names (character). See details for acceptable names.
#' @param genes vector of gene names (genes detected in proteomic data)
#' @details
#' The following tissues are available:
#' 
#' * liver
#' * intestine
#' * lung
#' * esophagus
#' * lymphoid tissue
#' * tongue
#' * gallbladder
#' * pancreas
#' * stomach 1
#' * adrenal gland
#' * placenta
#' * skin 1
#' * adipose tissue
#' * epididymis
#' * vagina
#' * parathyroid gland
#' * skeletal muscle
#' * retina
#' * testis
#' * smooth muscle
#' * brain
#' * kidney
#' * ovary
#' * bone marrow
#' * breast
#' * prostate
#' * pituitary gland
#' * ductus deferens
#' * seminal vesicle
#' * blood
#' * heart muscle
#' * cervix, uterine
#' * endometrium 1
#' * salivary gland
#' * fallopian tube
#' * thyroid gland
#' * urinary bladder
#' 
#' @import hash
#' @md
#' @export

get_hpa_lists <- function(tissue = NULL, genes = NULL){
  
  tissue.available = as.character(unique(hpa_table$RNA.tissue.specific))
  tissue.check = tissue[tissue %nin% tissue.available]
  
  if (length(tissue.check) > 0) stop(paste(tissue.check, 'is not an available tissue name. See ?get_hpa_lists for accepted inputs.')) 
  
  tissue.accepted <- unlist(ifelse(!is.null(tissue), list(hpa_table$RNA.tissue.specific %in% tissue), T))
  genes.accepted <- unlist(ifelse(!is.null(genes), list(hpa_table$gene %in% genes), T))
  table = hpa_table[tissue.accepted & genes.accepted,]
  
  return(table)
  
}





### to be deleted

# map to synonyms in data

#hpa_table = read.delim('~/Desktop/HPA/proteinatlas.tsv', stringsAsFactors = F)

#lov = strsplit(hpa_table$Gene.synonym, ', ')
#names(lov) = hpa_table$Gene
#synonyms = split(rep(names(lov), lengths(lov)), unlist(lov))



#hpa_table$RNA.tissue.specific.NX[nchar(hpa_table$RNA.tissue.specific.NX) == 0] <- 'none'

#hpa_table[c('Gene','RNA.tissue.specificity','RNA.tissue.specific.NX')]



#lov = strsplit(gsub('','',hpa_table$RNA.tissue.specific.NX), '\\;')
#names(lov) = hpa_table$Gene
#tissue = stack(split(rep(names(lov), lengths(lov)), unlist(lov)))
#tissue$ind <- as.character(tissue$ind)

#tissue.splitted <- as.data.frame(do.call(rbind, strsplit(tissue$ind, '\\:\\ ')))

#tissue <- cbind(tissue, tissue.splitted)
#tissue$ind <- NULL
#colnames(tissue) <- c("Gene", 'RNA.tissue.specific', "RNA.tissue.specific.NX")

#tissue.cat <- hpa_table[,c('Gene', 'Gene.synonym','RNA.tissue.specificity', 'RNA.tissue.distribution')]

#nt <- merge(tissue, tissue.cat, by = 'Gene')

#colnames(nt)

#nt <- nt[, c(1,4,2,3,5,6)]


# 
#normal <- read.csv('~/Desktop/HPA/normal_tissue.tsv', sep = '\t')
#normal <- normal[,c(2,3,5,6)]
#colnames(normal)[3:4] <- c('Protein.tissue.level', 'Protein.assay.reliability') 


#x = merge(nt, normal, by.x = c('Gene', 'RNA.tissue.specific'), by.y = c('Gene.name','Tissue'), all.x = T)

#final <- x[,c(1,3,2,4,5,6,7,8)]
#colnames(final)[1:2] <- c('gene', 'gene.synonym')

#hpa_table <- final

#save(hpa_table, file = 'hpa_table.rsa', compress = 'xz')
