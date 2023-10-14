# code to generate hpa_rna_table 
# last update: 2023-10-13

library(dplyr)

# download Human Protein Atlas data
url <- 'https://www.proteinatlas.org/download/proteinatlas.tsv.zip'
tmpFile <- tempfile('proteinatlas.tsv.zip') # store in R temp dir
utils::download.file(url,tmpFile)

dt <- data.table::fread(cmd=paste0('unzip -p ',tmpFile),
	header=T,sep='\t',check.names=T)

dim(dt) # 20162 rows x 89 columns
# each row is a unique Ensembl gene ID, a few with duplicate gene names

# tissue specificity categorization
#table(dt$RNA.tissue.specificity)
#        Group enriched Low tissue specificity           Not detected 
#                  1583                   8194                   1038 
#       Tissue enhanced        Tissue enriched 
#                  6197                   3150

# 10930 tissue elevated genes
# 8194 low tissue specificty genes
# 1038 not detected genes

# tissue elevated genes 
tissueDf <- subset(dt,RNA.tissue.specificity %in% c('Tissue enriched','Group enriched','Tissue enhanced')) %>%
	tidyr::separate_longer_delim(RNA.tissue.specific.nTPM,delim=';') %>%
	tidyr::separate_wider_delim(RNA.tissue.specific.nTPM,delim=': ',names=c('tissue','nTPM')) %>%
	dplyr::select(Gene,tissue) %>% unique()

tissues <- sort(unique(tissueDf$tissue))
length(tissues) # 36 tissues

detectedGenes <- sort(unique(subset(dt,RNA.tissue.specificity!='Not detected')$Gene))
length(detectedGenes) # 19114 unique gene names detected in ≥ 1 tissue

hpa_rna_table <- NULL
for (t in tissues) {
	hpa_rna_table <- dplyr::bind_rows(hpa_rna_table,
		tibble(tissue=t,gene=detectedGenes,
		significant=detectedGenes %in% subset(tissueDf,tissue==t)$Gene))
}
dim(hpa_rna_table) # 688104 rows x 3 columns

#table(subset(hpa_rna_table,significant)$tissue)
# check gene counts generally match: (may be off by a few genes due to collapsing duplicate gene names)
#https://www.proteinatlas.org/humanproteome/tissue/tissue+specific#tissue_elevated_genes

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(hpa_rna_table,file='data-raw/hpa_rna_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(hpa_rna_table, overwrite=T)

