# code to generate hgnc_group_table
# last update: 2023-10-02 

# download HGNC gene groups mapping data
url <- 'https://www.genenames.org/cgi-bin/genegroup/download-all'
dt <- data.table::fread(url,header=T) 

#names(dt)
# [1] "HGNC ID"          "Approved symbol"  "Approved name"    "Status"          
# [5] "Locus type"       "Previous symbols" "Alias symbols"    "Chromosome"      
# [9] "NCBI Gene ID"     "Ensembl gene ID"  "Vega gene ID"     "Group ID"        
#[13] "Group name" 

# create tibble object
hgnc_group_table <- dplyr::as_tibble(dt[,c('Approved symbol','Group ID','Group name')])
names(hgnc_group_table) <- c('Gene.symbol','Group.ID','Group.name')

#sum(is.na(hgnc_group_table)) # check no missing values
#dim(hgnc_group_table) # 30791 rows x 3 columns
#length(unique(hgnc_group_table$Gene.symbol)) # 25637 gene names
#length(unique(hgnc_group_table$Group.name)) # 1622 gene groups

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(hgnc_group_table,file='data-raw/hgnc_group_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(hgnc_group_table, overwrite=T) # use bzip2 compression by default

