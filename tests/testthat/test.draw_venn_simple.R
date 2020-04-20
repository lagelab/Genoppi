context('draw_venn_simple')


#futile.logger::
# for comparing images
#source('functions/compare_image.R')
func = 'draw_venn_simple'

# read in test data
data <- read_input("data/test.data.txt",header=T,sep="\t")$data
data <- suppressWarnings(calc_mod_ttest(data))
data = id_enriched_proteins(data)

test_that('inweb is correctly illustrated in venn diagram',{
  
  # get paths for image based comparison
  #id = 'A1'
  #paths = make_test_path(func, id)
  
  # get inweb hyper geometric data
  #inweb_output = get_inweb_list('BCL2')
  #inweb_list = data.frame(listName="InWeb", inweb_output)
  #inweb_intersect = data.frame(listName="InWeb", intersectN=T)
  
  # compile venn diagram information
  #hyper = calc_hyper(data, inweb_list, inweb_intersect, bait = 'BCL2')
  #hyper[['venn']][['A']] <- hyper$genes$InWeb$success_genes # pulldown
  #hyper[['venn' ]][['B']] <- hyper$genes$InWeb$sample_genes # inweb
  
  # draw venn
  #venn = draw_genoppi_venn(hyper$venn)
  #png(paths$res)
  #grid::grid.newpage()
  #grid::grid.draw(venn)
  #dev.off()
 
  # make comparison
  #expect_true(test_png_identity(paths))
  
})
