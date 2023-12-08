# code to generate bioplex_table
# last update: 2023-12-05
# NOTE: may need to manually recreate some ID mapping/conversion files below when rerunning

library(dplyr)

# UniProt accession -> gene name mapping from genoppi (from UniProt 2023-11-08 release)
load('data/accession_gene_table.rda')

# download BioPlex 3.0 data
url_hek <- 'https://bioplex.hms.harvard.edu/data/BioPlex_293T_Network_10K_Dec_2019.tsv'
url_hct <- 'https://bioplex.hms.harvard.edu/data/BioPlex_HCT116_Network_5.5K_Dec_2019.tsv'

hekDt <- data.table::fread(url_hek,header=T,sep='\t') %>%
	dplyr::mutate(CellLine='HEK293T')
dim(hekDt) # 118162 x 10
hctDt <- data.table::fread(url_hct,header=T,sep='\t') %>%
	dplyr::mutate(CellLine='HCT116')
dim(hctDt) # 70966 x 10

# merge HEK293T and HCT116 data
intDt <- dplyr::bind_rows(hekDt,hctDt) %>%
	# strip isoform suffix from Uniprot accessions
	dplyr::mutate(UniprotA=sapply(strsplit(UniprotA,'-'),'[[',1),
		UniprotB=sapply(strsplit(UniprotB,'-'),'[[',1))

# look up old UniProt accessions not found in current release (2023-11-08)
# using ID mapping interface: https://www.uniprot.org/id-mapping
#x <- unique(c(intDt$UniprotA,intDt$UniprotB))
#x <- x[!x %in% accession_gene_table$accession_number]
#length(x) # 312 accessions not found in accession_gene_table
#write.table(x,'data-raw/bioplex.old_uniprot.txt',quote=F,row.names=F,col.names=F)
# ID mapping results saved as: data-raw/bioplex.old_uniprot.idmapping_2023_12_05.tsv

# map old accessions -> current accessions -> gene names
oldDt <- data.table::fread('data-raw/bioplex.old_uniprot.idmapping_2023_12_05.tsv',header=T,sep='\t') %>%
	dplyr::select(oldAcc=From,newAcc=Entry) %>%
	dplyr::left_join(accession_gene_table,by=join_by(newAcc==accession_number)) %>%
	dplyr::select(oldAcc,gene) %>% unique() %>%
	# remove old accessions mapped to multiple gene names or not mapped to gene names
	dplyr::filter(!duplicated(oldAcc) & !is.na(gene)) %>% dplyr::rename(accession_number=oldAcc)
dim(oldDt) # 179 old accessions mapped to gene names

# map accessions for each pair of interactions to gene names
oldDt <- dplyr::bind_rows(oldDt,accession_gene_table) %>% dplyr::select(-all_genes)
intDt <- dplyr::left_join(intDt,oldDt,by=join_by(UniprotA==accession_number)) %>%
	dplyr::left_join(oldDt,by=join_by(UniprotB==accession_number)) %>%
	# remove self interactions (8) and interactions not mapped to gene names (8486)
	dplyr::filter(gene.x!=gene.y)
dim(intDt) # 180634 x 14

# sort Gene1 and Gene2 alphabetically, remove/collapse duplicate ints
bioplex_table <- data.frame(Gene1=apply(intDt[,c('gene.x','gene.y')],1,min),
	Gene2=apply(intDt[,c('gene.x','gene.y')],1,max),
	Score=format(intDt$pInt,digits=3),CellLine=intDt$CellLine) %>%
	# sort by CellLine, then decreasing Score (int confidence score)
	dplyr::as_tibble() %>%
	dplyr::arrange(desc(CellLine),desc(as.numeric(Score))) %>%
	# remove duplicate ints within each cell line (keep entry with max pInt)
	dplyr::distinct(Gene1,Gene2,CellLine,.keep_all=T) %>%
	# collapse duplicate ints across cell lines		
	dplyr::group_by(Gene1,Gene2) %>%
	dplyr::summarize_at(c('Score','CellLine'),function(x) paste(x,collapse=',')) 
dim(bioplex_table) # 159422 x 4
table(bioplex_table$CellLine)
# HEK293T: 90742, HCT116: 47544, both: 21136
length(unique(c(bioplex_table$Gene1,bioplex_table$Gene2))) # 14145

# interaction counts at each filtering step:
#	original: 189128
#	remove self ints: 189120
#	remove ummapped ints: 180634
#	remove duplicate ints within cell line: 180558
#	collapse duplicate ints across cell lines: 159422

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(bioplex_table,file='data-raw/bioplex_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(bioplex_table,overwrite=T) # use bzip2 compression by default

