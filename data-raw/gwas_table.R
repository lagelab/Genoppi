# code to generate gwas_table
# last update: 2023-11-15

library(dplyr)
 
# GWAS catalog v1.0.2 file (>300 MB) downloaded from: https://www.ebi.ac.uk/gwas/docs/file-downloads
# subset to relevant columns to save as smaller file (6 MB)
# cut -f 2,8,22,24,28,37 gwas_catalog_v1.0.2-associations_e110_r2023-11-08.tsv | \
# gzip > gwas_catalog_v1.0.2-associations_e110_r2023-11-08_subCols.tsv.gz

# filter to keep only entries mapped to current SNP ID (dbSNP build 154) 
gwas_table <- data.table::fread('data-raw/gwas_catalog_v1.0.2-associations_e110_r2023-11-08_subCols.tsv.gz',
	header=T,sep='\t',check.names=T) %>% dplyr::as_tibble() %>%
	dplyr::select(PUBMEDID,DISEASE.TRAIT,SNP=SNP_ID_CURRENT,P.VALUE,STUDY.ACCESSION) %>%
	dplyr::filter(SNP!='') %>% dplyr::mutate(SNP=paste0('rs',SNP),P.VALUE=as.numeric(P.VALUE)) %>% unique()

#dim(gwas_table) # 511955 rows x 5 columns
#length(unique(gwas_table$DISEASE.TRAIT)) # 25932
#length(unique(gwas_table$SNP)) # 262273

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(gwas_table,file='data-raw/gwas_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(gwas_table,overwrite=T) # use bzip2 compression by default

 
