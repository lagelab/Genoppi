# code to generate gnomad_table
# last update: 2023-11-14

library(dplyr)

# download gnomAD v4 gene constraint data
# relevant documentation: https://gnomad.broadinstitute.org/help/constraint
url <- 'https://storage.googleapis.com/gcp-public-data--gnomad/release/v4.0/constraint/gnomad.v4.0.constraint_metrics.tsv'
dt <- data.table::fread(url,header=T,sep='\t') %>% dplyr::as_tibble()

# filter out rows with missing gene names, non-MANE select/non-Ensembl transcripts, flagged outliers with no/extremem variant counts
# only keep pLI, LOEUF, missense Z-score, and synonymous Z-score columns for each gene
gnomad_table <- dplyr::filter(dt,!is.na(gene) & mane_select==TRUE & grepl('ENST',transcript) & grepl('\\[\\]',constraint_flags)) %>%
	dplyr::select(gene,pLI=lof.pLI,LOEUF=lof.oe_ci.upper,MisZ=mis.z_score,SynZ=syn.z_score)
	
#sum(is.na(gnomad_table)) # check no missing values
#dim(gnomad_table) # 16857 rows x 5 columns
#length(unique(gnomad_table$gene)) # 16857 genes

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(gnomad_table,file='data-raw/gnomad_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(gnomad_table,overwrite=T) # use bzip2 compression by default

