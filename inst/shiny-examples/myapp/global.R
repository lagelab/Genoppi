#library(shiny)
#library(ggrepel)
#library(VennDiagram)
#library(shinyjs)
#library(shinydashboard)
#library(hash)
#library(plotly)
#library(data.table)
#library(ggrepel)
#library(plyr) #unsure about plyr dependency.. test for now.
#library(VennDiagram)
#library(colourpicker)

# load genoppi if not attcahed
if (!'genoppi' %in% .packages()) devtools::load_all()
# load aprils functions for now
#source("~/Projects/04_genoppi/Genoppi-master/functions.R")
#source('aprils_functions.R')


# relative main directory
#main = '../../..'

main = system.file('extdata', package = 'genoppi')
marker_cols <- read.table(file.path(main, 'colors.txt'))
add_marker_cols <- read.table(file.path(main, 'colors_markers.txt'))
allowed_colors = unique(c('#41AB5D', 'red','green','#808080', 'blue', 'yellow', 'cyan', '#FF00FF', '#A52A2A'))
plotly_symbols = plotly::schema(F)$traces$scatter$attributes$marker$symbol$values
allowed_plotly_symbols = plotly_symbols[!grepl('^[0-9]+$', plotly_symbols)]

#up_to_hgnc <- read.table("data/HGNC_gene_to_UniProt_accession_number_Genoppi_ready.csv", header = T, sep = "\t", stringsAsFactors = F)
#prot_fam <- read.table(file.path(main, "protFams_genes_cols.txt"),  sep = "\t", quote = "", na.strings=c("","NA"), header = T, check.names = F)
#prot_fam_t <- data.frame(t(prot_fam))
futile.logger::flog.threshold(futile.logger::ERROR) # for ensuring venn diagrams does not make logs


myDownloadButton <- function(outputId, label = "Download", img = icon("camera")){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, img, label)
}
