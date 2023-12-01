# code to generate inweb_table
# last update: 2023-12-01
# NOTE: may need to manually recreate some ID mapping/conversion files below when rerunning

# source data containing input files used below:
# InWeb3: data-raw/inweb/InWeb3.zip (Kasper Lage)
# InWeb_IM: data-raw/inweb/inBio_Map_core_2016_09_12.zip
# (Li et al. Nat Methods 2017; https://zs-revelen.com/download)

library(dplyr)

# UniProt accession -> gene name mapping from genoppi (from UniProt 2023-11-08 release)
load('data/accession_gene_table.rda')

# Ensembl gene ID -> gene name mapping from GENCODE v43 (Ensembl 109)
url <- 'https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_43/gencode.v43.chr_patch_hapl_scaff.annotation.gtf.gz'
genDt <- data.table::fread(url,header=F,sep='\t') %>%
	dplyr::filter(V3=='gene') %>%
	tidyr::separate_wider_delim(V9,delim='; ',names=c('gene_id','gene_type','gene_name'),too_many='drop') %>%
	dplyr::mutate(gene_id=sapply(strsplit(gsub('gene_id |\"','',gene_id),'\\.'),'[',1),
		gene_type=gsub('gene_type |\"','',gene_type),
		gene_name=gsub('gene_name |\"','',gene_name)) %>%
	dplyr::select(ENSGID=gene_id,gene=gene_name) %>% unique()
dim(genDt) # 69175 x 2
length(unique(genDt$ENSGID))


# ----------------------------------------------------------------------------------------
### parse and map InWeb3 data

# node ID -> Ensembl gene ID and UniProt accession
nodeDt <- data.table::fread('data-raw/inweb/InWeb3_accessions.noa.gz',header=F,skip=1,sep=' ') %>%
	dplyr::select(node=V1,ensg=V3,uniprot=V3) %>%
	dplyr::mutate(ensg=stringr::str_match(ensg,'ensembl\\|(ENSG[0-9]+)')[,2]) %>%
	dplyr::mutate(uniprot=stringr::str_match(uniprot,'uniprot\\|([A-Z0-9]+)')[,2])
dim(nodeDt) # 22997 nodes
sum(is.na(nodeDt$ensg)) # all nodes have ENSG IDs
sum(is.na(nodeDt$uniprot)) # 1526 nodes with no UniProt accessions

# (1) map Ensembl ID -> gene name

# convert old ENSG IDs not in GENCODE v43 (Ensembl 109) to new IDs if available
# using Ensembl ID History Converter: https://useast.ensembl.org/Homo_sapiens/Tools/IDMapper
#x <- subset(nodeDt,!ensg %in% genDt$ENSGID)$ensg
#length(x) # 3550
#write.table(x,'data-raw/inweb/InWeb3_accessions.old_ensg.txt',quote=F,row.names=F,col.names=F)
# ID converter results saved as: data-raw/inweb/InWeb3_accessions.old_ensg_converted.csv

# old IDs -> new IDs -> gene names
oldDt <- data.table::fread('data-raw/inweb/InWeb3_accessions.old_ensg_converted.csv',header=T,sep=',') %>%
	dplyr::select(oldID='Requested ID',newID='Matched ID(s)') %>%
	dplyr::left_join(genDt,by=join_by(newID==ENSGID)) %>%
	dplyr::select(oldID,gene) %>% unique() %>%
	# remove old IDs mapped to multiple gene names or not mapped to gene names
	dplyr::filter(!duplicated(oldID) & !is.na(gene)) %>% dplyr::rename(ENSGID=oldID)
dim(oldDt) # 293 old IDs mapped to gene names

# node ID -> Ensembl ID -> gene name
oldDt <- dplyr::bind_rows(oldDt,genDt)
nodeDt <- dplyr::left_join(nodeDt,oldDt,by=join_by(ensg==ENSGID))

# (2) map Uniprot accession -> gene name

# convert old UniProt accessions not in current release (2023-11-08) to new accessions if available
# using ID mapping interface: https://www.uniprot.org/id-mapping
#x <- unique(subset(nodeDt,!is.na(uniprot) &
#	!uniprot %in% accession_gene_table$accession_number)$uniprot)
#write.table(x,'data-raw/inweb/InWeb3_accessions.old_uniprot.txt',quote=F,row.names=F,col.names=F)
# ID mapping results saved as: data-raw/inweb/InWeb3_accessions.old_uniprot.idmapping_2023_11_30.tsv

# old accessions -> new accessions -> gene names
oldDt <- data.table::fread('data-raw/inweb/InWeb3_accessions.old_uniprot.idmapping_2023_11_30.tsv',header=T,sep='\t') %>%
	dplyr::select(oldAcc=From,newAcc=Entry) %>%
	dplyr::left_join(accession_gene_table,by=join_by(newAcc==accession_number)) %>%
	dplyr::select(oldAcc,gene) %>% unique() %>%
	# remove old accessions mapped to multiple gene names or not mapped to gene names
	dplyr::filter(!duplicated(oldAcc) & !is.na(gene)) %>% dplyr::rename(accession_number=oldAcc)

# node ID -> UniProt accession -> gene name
oldDt <- dplyr::bind_rows(oldDt,accession_gene_table) %>% dplyr::select(-all_genes)
nodeDt <- dplyr::left_join(nodeDt,oldDt,by=join_by(uniprot==accession_number))

sum(is.na(nodeDt$gene.x)) # 3257 unmapped nodes using Ensembl ID
sum(is.na(nodeDt$gene.y)) # 5062 unmapped nodes using UniProt accession
sum(is.na(nodeDt$gene.x)&is.na(nodeDt$gene.y)) # 2090 unmapped nodes using both

# merge gene names mapped from Ensembl and UniProt, use Ensembl gene names first
nodeDt <- dplyr::mutate(nodeDt,gene=ifelse(!is.na(gene.x),gene.x,gene.y)) %>% 
	dplyr::select(node,gene)

# node-node interactions with scores
intDt <- data.table::fread('data-raw/inweb/InWeb3_score.eda.gz',header=F,skip=1,sep=' ') %>%
	dplyr::select(node1=V1,node2=V3,score=V5) %>%
	# map node to gene name
	dplyr::left_join(.,nodeDt,by=join_by(node1==node)) %>%
	dplyr::left_join(.,nodeDt,by=join_by(node2==node)) %>%	
	# remove self interactions (3812) and interactions not mapped to gene names (6953)
	dplyr::filter(gene.x!=gene.y)
dim(intDt) # 417664 x 7

# sort Gene1 and Gene2 alphabetically, sort rows by decreasing Score,
# remove rows with duplicate Gene1-Gene2 pairs (i.e. keep entry with max score)
iw3 <- data.frame(Gene1=apply(intDt[,c('gene.x','gene.y')],1,min),
	Gene2=apply(intDt[,c('gene.x','gene.y')],1,max),
	Score=intDt$score,Source='InWeb3') %>%
	as_tibble() %>% dplyr::arrange(-Score) %>%
	dplyr::distinct(Gene1,Gene2,.keep_all=T)
dim(iw3) # 385176 rows x 4 columns

# interaction counts at each filtering step:
#	original: 428429
#	remove self ints: 424617
#	remove unmapped ints: 417664
#	remove duplicate ints: 385176 


# ----------------------------------------------------------------------------------------
### parse and map InWeb_IM data

intDt <- data.table::fread('data-raw/inweb/InBio_Map_core_2016_09_12.psimitab.gz',header=F,sep='\t') %>%
	dplyr::select(uniprot1=V1,uniprot2=V2,score=V15) %>%
	dplyr::mutate(uniprot1=stringr::str_match(uniprot1,'uniprotkb:([A-Z0-9]+)')[,2],
		uniprot2=stringr::str_match(uniprot2,'uniprotkb:([A-Z0-9]+)')[,2],
		score=sapply(strsplit(score,'\\|'),'[[',1))
dim(intDt) # 625641 x 3

# look up old UniProt accessions not found in current release (2023-11-08)
# using ID mapping interface: https://www.uniprot.org/id-mapping
#x <- unique(c(intDt$uniprot1,intDt$uniprot2))
#x <- x[!x %in% accession_gene_table$accession_number]
#length(x) # 202
#write.table(x,'data-raw/inweb/InBio_Map_core_2016_09_12.old_uniprot.txt',quote=F,row.names=F,col.names=F)
# ID mapping results saved as: data-raw/inweb/InBio_Map_core_2016_09_12.old_uniprot.idmapping_2023_11_30.tsv

# map old accessions -> current accessions -> gene names
oldDt <- data.table::fread('data-raw/inweb/InBio_Map_core_2016_09_12.old_uniprot.idmapping_2023_11_30.tsv',header=T,sep='\t') %>%
	dplyr::select(oldAcc=From,newAcc=Entry) %>%
	dplyr::left_join(accession_gene_table,by=join_by(newAcc==accession_number)) %>%
	dplyr::select(oldAcc,gene) %>% unique() %>%
	# remove old accessions mapped to multiple gene names or not mapped to gene names
	dplyr::filter(!duplicated(oldAcc) & !is.na(gene)) %>% dplyr::rename(accession_number=oldAcc)

# map accessions for each pair of interactions to gene names
oldDt <- dplyr::bind_rows(oldDt,accession_gene_table) %>% dplyr::select(-all_genes)
intDt <- dplyr::left_join(intDt,oldDt,by=join_by(uniprot1==accession_number)) %>%
	dplyr::left_join(oldDt,by=join_by(uniprot2==accession_number)) %>%
	# remove self interactions (232) and interactions not mapped to gene names (548)
	dplyr::filter(gene.x!=gene.y)
dim(intDt) # 624861 x 5

# sort Gene1 and Gene2 alphabetically, sort rows by decreasing Score,
# remove rows with duplicate Gene1-Gene2 pairs (i.e. keep entry with max score)
im <- data.frame(Gene1=apply(intDt[,c('gene.x','gene.y')],1,min),
	Gene2=apply(intDt[,c('gene.x','gene.y')],1,max),
	Score=as.numeric(intDt$score),Source='InWeb_IM') %>%
	as_tibble() %>% dplyr::arrange(-Score) %>%
	dplyr::distinct(Gene1,Gene2,.keep_all=T)
dim(im) # 612036 rows x 4 columns

# interaction counts at each filtering step:
#	original: 625641
#	remove self ints: 625409
#	remove unmapped ints: 624861
#	remove duplicate ints: 612036 


# ----------------------------------------------------------------------------------------
### merge InWeb3 and InWeb_IM into inweb_table

inweb_table <- dplyr::bind_rows(iw3,im) %>%
	# keep InWeb_IM entry (i.e. score) if interaction found in both InWeb_IM and InWeb3
	dplyr::arrange(Gene1,Gene2,desc(Source)) %>% # put InWeb_IM before InWeb3
	#dplyr::group_by(Gene1,Gene2) %>%
	#dplyr::summarise(Score=toString(Score),Source=toString(Source)) %>%
	# checked: 871939 unique interactions
	# table(inweb_table$Source)
	# InWeb3 only: 259903, InWeb_IM only: 486763, overlap: 125273
	dplyr::distinct(Gene1,Gene2,.keep_all=T)

dim(inweb_table) # 871939 x 4
table(inweb_table$Source)
# InWeb_IM: 612036, InWeb3: 259903
length(unique(c(inweb_table$Gene1,inweb_table$Gene2)) # 17861


# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(inweb_table,file='data-raw/inweb_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(inweb_table,overwrite=T) # use bzip2 compression by default

