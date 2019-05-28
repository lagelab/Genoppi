#Created 9/10/16 by April Kim
#Global file for Shiny app Genoppi
library(shiny)
library(shinyjs)
library(plotly)
library(shinydashboard)
library(hash)
library(gplots)

load("data/InWeb_combined_Oct2019.RData")
load("data/proteinfam_loc_May2019.RData")
human_genome <- read.table("data/ensembl_homo_sapiens_genes.txt", header = T)
exac <- read.table("data/constrained_cleaned_exac_with_pHI_Aug26.txt", header = T, sep = "\t")
inweb_combined <- read.table("data/inweb_pooled.txt")
prot_fam <- read.table("data/protFams_genes_cols.txt", 
                       sep = "\t", quote = "", na.strings=c("","NA"), header = T, check.names = F)
marker_cols <- read.table("data/colors.txt")
add_marker_cols <- read.table("data/colors_markers.txt")

prot_fam_t <- data.frame(t(prot_fam))

myDownloadButton <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("camera"), label)
}