# code to generate accession_gene_table
# last update: 2023-09-27

library(data.table) # also requires R.utils for fread() to read in gz/bz2 files
library(dplyr)

# FTP download link for UniProt ID mapping table
url <- 'https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping.dat.gz'

# download ID mapping table, extract columns containing accession_number and gene names
dt <- fread(url,header=F) %>% filter(V2=='Gene_Name') %>% select(accession_number=V1,gene=V3)
	
# deal with accession_number mapped to multiple gene names:
#  collapse rows based on 'accession_number' column
#  'gene' column: 1st gene in all_genes
#  'all_genes': comma-delimited gene names, ordered alphabetically
accession_gene_table <- dt %>% group_by(accession_number) %>%	
	summarize(all_genes=paste(gene,collapse=',')) %>%
	mutate(gene=sapply(strsplit(all_genes,','),'[[',1)) %>%
	relocate(gene, .before=all_genes)

# save as gzipped tab-delimited file in data-raw/
fwrite(accession_gene_table,file='data-raw/accession_gene_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(accession_gene_table, overwrite=T) # use bzip2 compression by default

