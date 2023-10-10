# Code tested with R version 4.2.1
library(data.table) # also requires R.utils for fread() to read in gz/bz2 files
library(genoppi) # tested with v1.0.13 (for InWeb, BioPlex, iRefIndex data)
library(RCX) # tested with v1.2.2 (for reading Cytoscape CX format data)
library(tidyverse) # tested with v1.3.2
library(reshape2) # tested with v1.4.4

# ----------------------------------------------------------------------------------------
# read in HuRI data (HI-union network, Luck et al. Nature 2020)
# data from supplementary tables of Luck et al.

huri_genes <- read.csv("data-raw/HuRI_Luck2020_SuppTable2.txt",
    header = T, sep = "\t", stringsAsFactors = F
)
huri_genes$ENSG <- sapply(strsplit(huri_genes$ensembl_gene_id, "\\."), "[[", 1)

huri_ints <- read.csv("data-raw/HuRI_Luck2020_SuppTable11.txt",
    header = T, sep = "\t", stringsAsFactors = F
)

# remove self interaction
huri_ints <- subset(huri_ints, Ensembl_gene_id_a != Ensembl_gene_id_b)

# remove entries not mapped to gene names
# (11 unmapped ENSGs, likely due to older datasets mapped to deprecated IDs)
huri_ints <- subset(huri_ints, (Ensembl_gene_id_a %in% huri_genes$ENSG) &
    (Ensembl_gene_id_b %in% huri_genes$ENSG))

huri_table <- data.frame(
    Gene1 = huri_genes$symbol[match(huri_ints$Ensembl_gene_id_a, huri_genes$ENSG)],
    Gene2 = huri_genes$symbol[match(huri_ints$Ensembl_gene_id_b, huri_genes$ENSG)]
)

# save as gzipped tab-delimited file in data-raw/
fwrite(huri_table,file='data-raw/accession_gene_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(huri_table, overwrite=T) # use bzip2 compression by default