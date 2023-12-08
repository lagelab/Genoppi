# code to generate string_table
# last update: 2023-12-01

library(dplyr)

# download STRING genes/proteins info
url_genes <- 'https://stringdb-downloads.org/download/protein.info.v12.0/9606.protein.info.v12.0.txt.gz'
genDt <- data.table::fread(url_genes,header=T,sep='\t') %>%
	dplyr::select(protein_id='#string_protein_id',gene=preferred_name) 
dim(genDt) # 19699 x 2

# download STRING physical subnetwork
url_ints <- 'https://stringdb-downloads.org/download/protein.physical.links.v12.0/9606.protein.physical.links.v12.0.txt.gz'
intDt <- data.table::fread(url_ints,header=T,sep=" ") %>%
	# map protein IDs in intDt to gene names
	dplyr::left_join(genDt,by=join_by(protein1==protein_id)) %>%
	dplyr::left_join(genDt,by=join_by(protein2==protein_id))
dim(intDt) # 1477610 x 3
sum(is.na(intDt)) # no missing gene names
sum(intDt$gene.x==intDt$gene.y) # no self interactions

# sort Gene1 and Gene2 alphabetically, sort rows by decreasing Score
# remove rows with duplicate Gene1-Gene2 pairs (keeping entry with max score)
string_table <- data.frame(Gene1=apply(intDt[,c('gene.x','gene.y')],1,min),
	Gene2=apply(intDt[,c('gene.x','gene.y')],1,max),
	Score=intDt$combined_score/1000) %>% # scores between 0-1 (as described in STRING doc)
	as_tibble() %>% dplyr::arrange(-Score) %>%
	dplyr::distinct(Gene1,Gene2,.keep_all=T)
	# all duplicate ints have same scores
	# each int pair was stored twice in source file, i.e. bidirectionally
	# so this step reduce # of rows by half
dim(string_table) # 738805 x 3
length(unique(c(string_table$Gene1,string_table$Gene2))) # 18767 unique genes

# save as gzipped tab-delimited file in data-raw/ç
data.table::fwrite(string_table,file='data-raw/string_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(string_table,overwrite=T) # use bzip2 compression by default

