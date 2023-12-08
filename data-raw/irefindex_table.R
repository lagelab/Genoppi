# code to generate irefindex_table
# last update: 2023-12-05

library(dplyr)

# iRefIndex v20.0 human data downloaded from:
# https://storage.googleapis.com/irefindex-data/archive/release_20.0/psi_mitab/MITAB2.6/9606.mitab.08-28-2023.txt.zip
# subset columns (cut -f 1-15,53) and save as gzipped file: data-raw/irefindex_9606.mitab.08-28-2023.subCols.txt.gz
# (full file too big for github repo)
rawDt <- data.table::fread('data-raw/irefindex_9606.mitab.08-28-2023.subCols.txt.gz',header=T,sep='\t')
dim(rawDt) # 4195462 x 6

# only keep binary interactions between human proteins
intDt <- dplyr::filter(rawDt,edgetype=='X' &
		taxa=='taxid:9606(Homo sapiens)' & taxb=='taxid:9606(Homo sapiens)') %>%
	# extract gene names (checked that all entries with 'hgnc' were extracted)
	# and confidence scores (i.e. number of PMIDs supporting the int)
	dplyr::mutate(Gene1=stringr::str_match(aliasA,'hgnc:([A-Za-z0-9\\-]+)')[,2],
		Gene2=stringr::str_match(aliasB,'hgnc:([A-Za-z0-9\\-]+)')[,2],
		Score=as.integer(stringr::str_match(confidence,'np:([0-9]+)')[,2])) %>%
	# remove self interactions (41297) and ints not mapped to gene names (294419)
	dplyr::filter(Gene1!=Gene2)	
dim(intDt) # 2291825 x 9

# sort Gene1 and Gene2 alphabetically, sort rows by decreasing Score,
# remove rows with duplicate Gene1-Gene2 pairs (i.e. keep entry with max score)
irefindex_table <- data.frame(Gene1=apply(intDt[,c('Gene1','Gene2')],1,min),
	Gene2=apply(intDt[,c('Gene1','Gene2')],1,max),Score=intDt$Score) %>%
	dplyr::as_tibble() %>% dplyr::arrange(-Score) %>%
	dplyr::distinct(Gene1,Gene2,.keep_all=T)
dim(irefindex_table) # 776608 x 3
length(unique(c(irefindex_table$Gene1,irefindex_table$Gene2))) # 18166

# interaction counts at each filtering step:
#	original: 4195462
#	remove non-binary interactions: 2945942
#	remove non-human proteins: 2627541
#	remove self ints: 2586244
#	remove ints with no gene names: 2291825
#	remove duplicate ints: 776608

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(irefindex_table,file='data-raw/irefindex_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(irefindex_table,overwrite=T) # use bzip2 compression by default

