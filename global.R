#Created 9/10/16 by April Kim
#Global file for Shiny app Genoppi
library(shiny)
library(shinyjs)
library(plotly)

# inweb <- read.table("data/all_gene_names_in_InWeb_IM")
human_genome <- read.table("data/ensembl_homo_sapiens_genes.txt", header = T)
exac <- read.table("data/constrained_cleaned_exac_with_pHI_Aug26.txt", header = T, sep = "\t")
inweb_combined <- read.table("data/InWeb_pooled_all_genes.txt")
prot_fam <- read.table(gzfile("data/prot_fams_and_genes.txt.gz"), sep = "\t", quote = "")
