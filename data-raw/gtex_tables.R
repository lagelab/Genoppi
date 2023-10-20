# code to generate gtex_rna_table, gtex_brain_table, gtex_protein_table
# last update: 2023-10-13

library(dplyr)

# download GENCODE v43 GTF for Ensembl gene ID to gene name mapping
url <- 'https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_43/gencode.v43.chr_patch_hapl_scaff.annotation.gtf.gz'
genDt <- data.table::fread(url,header=F,sep='\t')

# extract relevant rows and columns for protein-coding genes
genDt <- dplyr::filter(genDt,V3=='gene') %>%
	tidyr::separate_wider_delim(V9,delim='; ',names=c('gene_id','gene_type','gene_name'),too_many='drop') %>%
	dplyr::mutate(gene_id=sapply(strsplit(gsub('gene_id |\"','',gene_id),'\\.'),'[',1),
		gene_type=gsub('gene_type |\"','',gene_type),
		gene_name=gsub('gene_name |\"','',gene_name)) %>%
	dplyr::filter(gene_type=='protein_coding') %>% dplyr::select(ENSGID=gene_id,gene=gene_name) %>% unique() 
dim(genDt) # 22850 x 2
length(unique(genDt$ENSGID)) # 22850 unique gene IDs
length(unique(genDt$gene)) # 20263 unique gene names


# GTEx RNA data (Finucane et al. 2018)
# original files available at (need to pay for download with google billing project):
# https://console.cloud.google.com/storage/browser/broad-alkesgroup-public-requester-pays/LDSCORE/LDSC_SEG_ldscores/tstats/
# downloaded GTEx.tstat.tsv and GTEx_brain.tstat.tsv on 2023-10-13
# gzip and store in data-raw/gtex
# also add tissue name and category mapping table (from Supp Table 2 of Finucane et al.)


# (1) create gtex_rna_table
tissueDt <- data.table::fread('data-raw/gtex/LDSCORE-LDSC_SEG_ldscores-tstats-GTEx.tstat.tsv.gz',header=T,sep='\t',check.names=T)
dim(tissueDt) # 24842 genes (ENSGID) across 53 tissues

tissueDt <- dplyr::inner_join(tissueDt,genDt) %>% dplyr::filter(!duplicated(gene))
dim(tissueDt) # 17589 ENSGID mapped to unique protein-coding gene names 
# filter step only removed 1 row with duplicate gene name: NPIPA9 (2 rows for NPIPA9 have correlation > 0.99, kept first one)

# cleaner tissue name and cateogry
nameDt <- data.table::fread('data-raw/gtex/LDSCORE-LDSC_SEG_ldscores-tstats-GTEx.tissue.names.tsv.gz',header=T,sep='\t')

# define and save tissue-specific genes
top10_cutoff <- floor(nrow(tissueDt)*0.1) # number of genes in top 10%
gtex_rna_table <- NULL
for (t in names(tissueDt)[!names(tissueDt) %in% c('ENSGID','gene')]) {
	# rank genes by decreasing t-statistic, save list of genes in top 10%
	tGenes <- tissueDt$gene[order(tissueDt[,get(t)],decreasing=T)][1:top10_cutoff]
	gtex_rna_table <- dplyr::bind_rows(gtex_rna_table,
		tibble(tissue=subset(nameDt,tissue==t)$tissue_name,
		category=subset(nameDt,tissue==t)$tissue_category,
		gene=tissueDt$gene, significant=tissueDt$gene %in% tGenes))
}
dim(gtex_rna_table) # 932217 rows x 4 columns

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(gtex_rna_table,file='data-raw/gtex_rna_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(gtex_rna_table, overwrite=T)


# (2) create gtex_brain_table
brainDt <- data.table::fread('data-raw/gtex/LDSCORE-LDSC_SEG_ldscores-tstats-GTEx_brain.tstat.tsv.gz',header=T,sep='\t',check.names=T)
dim(brainDt) # 24842 genes (ENSGID) across 13 brain tissues/regions

brainDt <- dplyr::inner_join(brainDt,genDt) %>% dplyr::filter(!duplicated(gene))
dim(brainDt) # 17589 ENSGID mapped to unique protein-coding gene names 

# define and save tissue-specific genes
top10_cutoff <- floor(nrow(brainDt)*0.1) # number of genes in top 10%
gtex_brain_table <- NULL
for (t in names(brainDt)[!names(brainDt) %in% c('ENSGID','gene')]) {
	# rank genes by decreasing t-statistic, save list of genes in top 10%
	tGenes <- brainDt$gene[order(brainDt[,get(t)],decreasing=T)][1:top10_cutoff]
	gtex_brain_table <- dplyr::bind_rows(gtex_brain_table,
		tibble(tissue=subset(nameDt,tissue==t)$tissue_name,
		gene=brainDt$gene, significant=brainDt$gene %in% tGenes))
}
dim(gtex_brain_table) # 228657 rows x 4 columns

# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(gtex_brain_table,file='data-raw/gtex_brain_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(gtex_brain_table, overwrite=T)


# ----------------------------------------------------------------------------------------	
# GTEx protein data (Jiang et al. 2020)

# download Table S4 of Jiang et al.
url <- 'https://ars.els-cdn.com/content/image/1-s2.0-S0092867420310783-mmc5.xlsx'
downFile <- 'data-raw/gtex/Jiang2020_TableS4.xlsx'
download.file(url,downFile)

# can also use Table S2 to derive the same info (used for Genoppi v1.0.0)
# url <- 'https://ars.els-cdn.com/content/image/1-s2.0-S0092867420310783-mmc3.xlsx'

prtDt <- readxl::read_excel(downFile,sheet='A enrichment comparison',skip=2)
names(prtDt)[1] <- 'ENSGID'
dim(prtDt) # 12627 genes (ENSGID)

# remap gene name (instead of using hgnc_name column) using GENCODE v43 for consistency
# (also avoid propogating gene name date conversion errors in oriiginal excel file)
prtDt <- dplyr::inner_join(prtDt,genDt) %>% dplyr::filter(!duplicated(gene))
dim(prtDt) # 12325 ENSGID mapped to unique gene names
# only 1 duplicated gene name (PINX1), kept 1st row that is tissue-specific (2nd row is not)

detectedGenes <- sort(unique(prtDt$gene))
length(detectedGenes) #12325

# tissue enriched genes
prtDt <- subset(prtDt,prt_ench_category %in% c('prt_enriched_not_spec','prt_specific')) %>%
	tidyr::separate_longer_delim(prt_ench_tissue_score,delim=';') %>%
	tidyr::separate_wider_delim(prt_ench_tissue_score,delim=':',names=c('tissue','TSscore')) %>%
	dplyr::select(gene,tissue)

tissues <- sort(unique(prtDt$tissue))
length(tissues) #32

gtex_protein_table <- NULL
for (t in tissues) {
	gtex_protein_table <- dplyr::bind_rows(gtex_protein_table,
		tibble(tissue=t,gene=detectedGenes,
		significant=detectedGenes %in% subset(prtDt,tissue==t)$gene))
}
dim(gtex_protein_table) # 394400 x 3
 
# save as gzipped tab-delimited file in data-raw/
data.table::fwrite(gtex_protein_table,file='data-raw/gtex_protein_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(gtex_protein_table, overwrite=T)



