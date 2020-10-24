library(genoppi)

# load genoppi if not attcahed (developer)
if (!'genoppi' %in% .packages()) devtools::load_all()
main = system.file('extdata', package = 'genoppi')
genoppi.ver = as.character(packageVersion("genoppi"))
  
# setup the colors used in the app
marker_cols <- read.table(file.path(main, 'colors.txt'))
add_marker_cols <- read.table(file.path(main, 'colors_markers.txt'))
add_marker_gradient <- readLines(file.path(main, 'color_markers_gradient.txt'))
allowed_colors = c(add_marker_gradient,'#808080', '#A52A2A')
spinner_type = 8

# load documentation
documentation = lapply(list.files('documentation/', full.names = T), readLines)
documentation = paste(documentation, collapse = '<br><br>')

##---------------------------
# global default variables

# global genoppi variables
max.genesets = 100
max.nchar.legend = 35
global.color.pathway.seed = 1

# genoppi plotly dimensions
global.basic.volcano.width = 650*0.95
global.basic.volcano.height = 550*0.8
global.basic.scatter.width = 650*0.95
global.basic.scatter.height = 550*0.8
global.integrated.volcano.width = 650
global.integrated.volcano.height = 550
global.genesets.volcano.width = 875
global.genesets.volcano.height = 500

# genoppi download dimensions
global.img.volcano.download.width = 12
global.img.volcano.download.height = 8
global.img.scatter.download.width = 12
global.img.scatter.download.height = 8

# upload size
options(shiny.maxRequestSize=30*1024^2) # max 30MB upload

# what is the allowed palette of symbols in plotly
plotly_symbols = table_symbols()$symbol
#plotly_symbols = plotly::schema(F)$traces$scatter$attributes$marker$symbol$values
allowed_plotly_symbols = plotly_symbols[!grepl('^[0-9]+$', plotly_symbols)]

# what file types are acccepted
files_accepted = c('text/csv',
                   'text/comma-separated-values',
                   'text/tab-separated-values',
                   'text/plain',
                   '.csv',
                   '.tsv')

# for ensuring venn diagrams does not make logs
futile.logger::flog.threshold(futile.logger::ERROR) 

# save example data so that it can be loaded in the app
data("example_data")
data("example_data2")
data("example_data3")
example_file = tempfile(fileext = '.txt')
example_file2 = tempfile(fileext = '.txt')
example_file3 = tempfile(fileext = '.txt')
write.table(example_data, example_file, quote = F, row.names = F, sep = '\t')
write.table(example_data2, example_file2, quote = F, row.names = F, sep = '\t')
write.table(example_data3, example_file3, quote = F, row.names = F, sep = '\t')


#
myDownloadButton <- function(outputId, label = "Download", img = icon("camera")){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, img, label)
}

# tmp load data
data('hpa_table')
data('gtex_table')


