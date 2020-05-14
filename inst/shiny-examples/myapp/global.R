library(genoppi)

# load genoppi if not attcahed (developer)
if (!'genoppi' %in% .packages()) devtools::load_all()
main = system.file('extdata', package = 'genoppi')

# setup the colors used in the app
marker_cols <- read.table(file.path(main, 'colors.txt'))
add_marker_cols <- read.table(file.path(main, 'colors_markers.txt'))
add_marker_gradient <- readLines(file.path(main, 'color_markers_gradient.txt'))
allowed_colors = c(add_marker_gradient,'#808080', '#A52A2A')

# global genoppi variables
max.genesets = 100
max.nchar.legend = 35
global.color.pathway.seed = 1
global.img.volcano.download.width = 12
global.img.volcano.download.height = 8
global.img.scatter.download.width = 8
global.img.scatter.download.height = 8

# what is the allowed palette of symbols in plotly
plotly_symbols = plotly::schema(F)$traces$scatter$attributes$marker$symbol$values
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

#
myDownloadButton <- function(outputId, label = "Download", img = icon("camera")){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, img, label)
}
