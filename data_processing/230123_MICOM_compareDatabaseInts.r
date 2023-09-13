##########################################################################################
## Assess overlap between PPIs identified in our study vs. known PPIs in
## interaction databases (InWeb, BioPlex, iRefIndex, HuRI, STRING, PCNet)
##
## Author: Yu-Han Hsu
##########################################################################################

rm(list=ls())
setwd('~/Google Drive/My Drive/LageLab/MICOM')

# Code tested with R version 4.2.2
library(genoppi) # tested with v1.0.13 (for InWeb, BioPlex, iRefIndex data)
library(RCX) # tested with v1.2.2 (for reading Cytoscape CX format data)
library(tidyverse) # tested with v1.3.2 
library(reshape2) # tested with v1.4.4 

# ----------------------------------------------------------------------------------------
# read in HuRI data (HI-union network, Luck et al. Nature 2020)
# data from supplementary tables of Luck et al.

huri_genes <- read.csv('PPI_Databases/HuRI_Luck2020_SuppTable2.txt',
	header=T,sep='\t',stringsAsFactors=F)
huri_genes$ENSG <- sapply(strsplit(huri_genes$ensembl_gene_id,'\\.'),'[[',1)

huri_ints <- read.csv('PPI_Databases/HuRI_Luck2020_SuppTable11.txt',
	header=T,sep='\t',stringsAsFactors=F)

# remove self interaction
huri_ints <- subset(huri_ints,Ensembl_gene_id_a!=Ensembl_gene_id_b)

# remove entries not mapped to gene names
# (11 unmapped ENSGs, likely due to older datasets mapped to deprecated IDs)
huri_ints <- subset(huri_ints,(Ensembl_gene_id_a %in% huri_genes$ENSG) &
	(Ensembl_gene_id_b %in% huri_genes$ENSG))

huri_table <- data.frame(
	Gene1=huri_genes$symbol[match(huri_ints$Ensembl_gene_id_a,huri_genes$ENSG)],
	Gene2=huri_genes$symbol[match(huri_ints$Ensembl_gene_id_b,huri_genes$ENSG)])

# ----------------------------------------------------------------------------------------
# read in STRING data (physical subnetwork, Szklarczyk et al. Nucleic Acids Res 2021)
# downloaded from: https://string-db.org/cgi/download?sessionId=bpij0JN28bsF
# restricting to Homo sapiens, physical interactions

# first 3 columns of original file: STRING_9606.protein.info.v11.5.txt.gz
string_genes <- read.table('PPI_Databases/STRING_9606.protein.info.v11.5.short.txt.gz',
	header=F,sep='\t',stringsAsFactors=F)
names(string_genes) <- c('protein_id','gene_name','protein_size')
# ~400 genes with ENSG IDs in gene name column, keep as is for now

string_ints <- read.table('PPI_Databases/STRING_9606.protein.physical.links.v11.5.txt.gz',
	header=T,sep=' ',stringsAsFactors=F)

string_table <- data.frame(
	Gene1=string_genes$gene_name[match(string_ints$protein1,string_genes$protein_id)],
	Gene2=string_genes$gene_name[match(string_ints$protein2,string_genes$protein_id)])

# ----------------------------------------------------------------------------------------
# read in PCNet data (Huang et al. Cell Systems 2018)
# downloaded from NDEx (UUID: f93f402c-86d4-11e7-a10d-0ac135e8bacf)

rcx <- readCX('PPI_Databases/PCNet_Huang2018.cx')
rcx$metaData # 19781 nodes, 2724724 edges
head(rcx$nodes) # id name represents
head(rcx$edges) # id source target
pcnet_table <- data.frame(Gene1=rcx$nodes$name[match(rcx$edges$source,rcx$nodes$id)],
	Gene2=rcx$nodes$name[match(rcx$edges$target,rcx$nodes$id)])
	
# ----------------------------------------------------------------------------------------
# function for returning interactors of a bait in a interaction database
get_ints_list <- function(bait,databaseDf){
	dbDf <- NULL

	# all genes in the databaseDf
	dbGenes <- unique(c(databaseDf$Gene1,databaseDf$Gene2))
	
	if (bait %in% dbGenes) { 
	
		# interactor genes
		ints <- setdiff(unique(c(subset(databaseDf,Gene1==bait)$Gene2,
			subset(databaseDf,Gene2==bait)$Gene1)),bait)
		
		# df with gene and significant columns
		# (significant==T for ints, F otherwise)
		dbDf <- data.frame(gene=setdiff(dbGenes,bait))
		dbDf$significant <- dbDf$gene %in% ints
	}
	
	return(dbDf)
}

# ----------------------------------------------------------------------------------------
# read in index gene interactors from our study
# assess overlap with known interactors in PPI databases


intDf <- read.table('InteractorLists/210406_MICOM_MasterInteractorTable.txt',
	header=T,sep='\t',stringsAsFactors=F)

prefixes <- c('ARHGEF26_EC','BCAS3_EC','EDN1_EC','FLT1_EC','FN1_EC',
	'HDAC9_EC','JCAD_EC','PHACTR1_EC','PLPP3_EC',
	'ADAMTS7_SMC','ARHGEF26_SMC','BCAS3_SMC','EDN1_SMC','EDNRA_SMC',
	'FN1_SMC','HDAC9_SMC','JCAD_SMC','PHACTR1_SMC','PLPP3_SMC')

dbList <- list(InWeb=inweb_table,BioPlex=bioplex_table,iRefIndex=irefindex_table,
	HuRI=huri_table,STRING=string_table,PCNet=pcnet_table)

overlapRes <- NULL
intAnnDf <- NULL

# iterate through int lists (BAIT_CELLTYPE)
for (prefix in prefixes) {
	#prefixFile <- paste('InteractorLists/',prefix,'.InteractorTable.txt',sep="")
	#prefixTable <- read.table(prefixFile,header=T,sep="\t",stringsAsFactors=F)
	prefixTable <- subset(intDf,ListName==prefix)
	
	bait <- strsplit(prefix,'_')[[1]][1]
	
	tempDf <- subset(prefixTable,Interactor)[,c("CellType","Bait","GeneName")]

	# get known ints in databases
	dbListDf <- NULL
	for (db in names(dbList)) {
		dbDf <- get_ints_list(bait,dbList[[db]])
		if (!is.null(dbDf)) { # if bait in database
			dbListDf <- rbind(dbListDf,data.frame(listName=db,dbDf))
							
			tempDf[db] <- tempDf$GeneName %in% subset(dbDf,significant)$gene
		
		} else{		
			tempDf[db] <- FALSE
		}
	}

	# table of interactors with T/F annotation for each database	
	intAnnDf <- rbind(intAnnDf,tempDf)
	
	# overlap enrichment test
	preDf <- data.frame(gene=prefixTable$GeneName,significant=prefixTable$Interactor)
	intersectDf <- data.frame(listName=unique(dbListDf$listName),intersectN=T)
	overlapRes <- rbind(overlapRes,data.frame(Dataset=prefix,
		calc_hyper(preDf,dbListDf,intersectDf,bait)$statistics))

}

# output overlap results table
write.table(overlapRes,
	'230123_MICOM_DatabaseIntsOverlapStats.txt',quote=F,row.names=F,sep='\t')


# combined EC, SMC, EC-SMC pie charts
pieDf <- rbind(intAnnDf,data.frame(CellType='EC-SMC',unique(intAnnDf[,-1]))) |>
	melt(id.vars=c('CellType','Bait','GeneName')) |>
	group_by(CellType,variable) |>
	count(value) |>
	# Calculate percentage as n / sum of n by variable
	mutate(perc = 100* n / sum(n))

pdf('Plots/230123_MICOM_DatabaseIntsPieCharts.pdf',width=7,height=3)

for (ct in c('EC','SMC','EC-SMC')) {
	
	p <- ggplot(subset(pieDf,CellType==ct),aes(x="",y=n,fill=value)) +
	facet_wrap(.~variable,nrow=1) +
	geom_bar(width=1, stat="identity", color="black") +
	coord_polar("y") + scale_fill_manual(name="Reported", values=c("grey","blue")) +
	ggtitle(paste('Combined PPI network in',ct)) +
	theme_void() + 
	theme(legend.position='bottom',plot.title=element_text(hjust=0.5,vjust=2))
	
	print(p)
}

dev.off()

# output pie chart stats
write.table(pieDf,'230123_MICOM_DatabaseIntsPieChartStats.txt',
	quote=F,row.names=F,sep='\t')


# collapse and output interactor table with database annotations
intAnnDf <- intAnnDf |> group_by(Bait,GeneName) |>
	summarise(CellTypes=toString(CellType),
	InWeb=unique(InWeb),BioPlex=unique(BioPlex),iRefIndex=unique(iRefIndex),
	HuRI=unique(HuRI),STRING=unique(STRING),PCNet=unique(PCNet))

write.table(intAnnDf,'230123_MICOM_IntDatabaseAnnotations.txt',
	quote=F,row.names=F,sep='\t')

