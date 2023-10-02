# code to generate goa_mf_table, goa_cc_table, goa_bp_table
# last update: 2023-10-02

# download GO annotations
# GAF format: https://geneontology.org/docs/go-annotation-file-gaf-format-2.2/
url <- 'https://geneontology.org/gene-associations/goa_human.gaf.gz'
dt <- data.table::fread(url,header=F) # should automatically skip comment lines starting with '!'
dim(dt) # 633586 rows x 17 columns

dt <- dplyr::select(dt,Gene.symbol=V3,GO.ID=V5,GO.type=V9) 

# download GO term ID to name mapping data
url <- 'https://purl.obolibrary.org/obo/go/go-basic.obo'
idLines <- readLines(url)

types <- stringr::str_extract(idLines,'^\\[(.+)\\]$',group=1)
ids <- stringr::str_extract(idLines,'^id: (.+)$',group=1)
names <- stringr::str_extract(idLines,'^name: (.+)$',group=1)
mapDf <- dplyr::tibble(Ann.type=types[!is.na(types)],
	GO.ID=ids[!is.na(ids)],GO.name=names[!is.na(names)])
mapDf <- subset(mapDf,Ann.type=='Term')[,c('GO.ID','GO.name')]

#sum(!dt$GO.ID %in% mapDf$GO.ID) # check all GO.ID are in mapping table
dt <- dplyr::inner_join(dt,mapDf)
#dim(dt) # 633586 rows x 4 columns

# -------------------------------------------------------------------------------------
goa_mf_table <- dplyr::as_tibble(
	subset(dt,GO.type=='F')[,c('Gene.symbol','GO.ID','GO.name')])

#dim(goa_mf_table) # 292611 rows x 3 columns
#length(unique(goa_mf_table$Gene.symbol)) # 18172 genes
#length(unique(goa_mf_table$GO.ID)) # 4614 GO terms

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(goa_mf_table,file='data-raw/goa_mf_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(goa_mf_table, overwrite=T) # use bzip2 compression by default

# -------------------------------------------------------------------------------------
goa_cc_table <- dplyr::as_tibble(
	subset(dt,GO.type=='C')[,c('Gene.symbol','GO.ID','GO.name')])

#dim(goa_cc_table) # 179867 rows x 3 columns
#length(unique(goa_cc_table$Gene.symbol)) # 18868 genes
#length(unique(goa_cc_table$GO.ID)) # 1800 GO terms

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(goa_cc_table,file='data-raw/goa_cc_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(goa_cc_table, overwrite=T) # use bzip2 compression by default

# -------------------------------------------------------------------------------------
goa_bp_table <- dplyr::as_tibble(
	subset(dt,GO.type=='P')[,c('Gene.symbol','GO.ID','GO.name')])

#dim(goa_bp_table) # 161108 rows x 3 columns
#length(unique(goa_bp_table$Gene.symbol)) # 17775 genes
#length(unique(goa_bp_table$GO.ID)) # 12335 GO terms

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(goa_bp_table,file='data-raw/goa_bp_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(goa_bp_table, overwrite=T) # use bzip2 compression by default

