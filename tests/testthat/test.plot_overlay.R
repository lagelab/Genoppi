context('plot_overlay')

# for comparing images
#library(png)
source('functions/compare_image.R')
#func = 'plot_overlay'

# read in test data
df <- read_input("data/test.data2.txt", sep="\t")$data
df <- suppressWarnings(calc_mod_ttest(df))


test_that('simple overlay of a bait',{
  
  # a few different test cases
  id = 'A1'
  df = id_significant_proteins(df)
  p = plot_volcano_basic(df, plot_segments = T) + ggtitle('BCL2 vs IgG in GPiNs') 
  p = plot_overlay(p, as.bait('BCL2')) ## testcase here..
  
  # test that result and reference are same
  #paths = make_test_path(func, id)
  #ggsave(paths$res, p, width = 5, height = 5)
  #test = test_png_identity(paths)
  #expect_true(test) #check_plot(func, id)
  
  #save_gg_reference(p, func, id)
  #expect_true(compare_with_reference(p, func, id))
  
  
  id = 'A2'
  # a few different test cases
  df <- id_significant_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df, plot_segments = T) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # Generate random dataset
  ref1= data.frame(gene=c('APPL1', 'RAB7A'),col_significant='cyan',col_other='grey')
  ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey')
  reference = list(ref1, ref2)
  names(reference) = c('genes (I)', 'genes (II)')
  
  # overlay second list
  p1 = plot_overlay(p1, reference) # testcase here..
  expect_true(!is.null(p1))
  #ggsave(paths$res, p1, width = 5, height = 5)
  #paths = make_test_path(func, id) #   
  #test = test_png_identity(paths)
  #expect_true(test) #check_plot(func, id)
  

})

test_that('inweb and gnomad overlay',{
  
  # setup basic test
  id = 'B1'
  df <- id_significant_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df, plot_segments = T)

  # overlay with square bait
  bait = as.bait('BCL2')
  bait$bait$shape = 22
  p1 = plot_overlay(p, bait)
  expect_true(!is.null(p1))
  
  # overlay with diamond inweb
  inweb = get_inweb_list('BCL2')
  inweb = list(inweb=inweb[inweb$significant, ])
  inweb$inweb$shape = 23
  p2 = plot_overlay(p1, inweb, label = F)
  expect_true(!is.null(p2))
  
  # overlay with gnomad
  id = 'B2'
  gnomad = gnomad_table[gnomad_table$gene %in% df$gene & gnomad_table$pLI == 1, ]
  gnomad = list(gnomad = data.frame(gnomad, col_significant = 'blue', col_other = 'grey'))
  p3 = suppressWarnings(plot_overlay(p2, gnomad, label = F))
  expect_true(!is.null(p3))
  #expect_true(compare_with_reference(p3, func, id))
  
})

test_that('ggplot is built correctlty',{
  
  id = 'C1'
  df = id_significant_proteins(df)
  p = plot_volcano_basic(df, plot_segments = T)

  # check for both manuel shape and fill
  expect_equal(unlist(lapply(p$scales$scales, function(x) x[['aesthetics']])), c('fill','shape','colour'))
  
  # build plot
  g = ggplot_build(p)  
  
  # check plot
  expect_equal(unique(g$data[[1]]$fill), c("#41AB5D","grey"))
  expect_equal(length(g$data), 3) # 3 objects: 2 lines and 1 scatter
  
})


test_that('invalid columns in overlay gives warning and errors',{
  
  # invalid columns in overlay gives warning
  df <- id_significant_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df, plot_segments = T) + ggtitle('BCL2 vs IgG in GPiNs') 
  ref1= data.frame(gene=c('APC2', 'RAB7A'),col_significant='cyan',col_other='grey', col_invalid = T)
  ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey', col_invalid = T)
  reference = list(ref1, ref2)
  names(reference) = c('SCZ genelist', 'ASD genelist')
  
  # check invalid input 
  expect_error(plot_overlay(p, reference$`SCZ genelist`)) # must be a named list
  
})

test_that('ggplot shapes are correctly translated to plotly symbols',{
  
  # read data
  df <- read_input("data/test.data2.txt", sep="\t")$data
  df <- suppressWarnings(calc_mod_ttest(df))
  
  # setup basic test
  id = 'D1'
  df <- id_significant_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df, plot_segments = T)
  
  # overlay with square bait
  bait = as.bait('BCL2')
  bait$bait$shape = 22
  p1 = plot_overlay(p, bait)
  
  # overlay with diamond inweb
  inweb = get_inweb_list('BCL2')
  inweb = list(inweb=inweb[inweb$significant, ])
  inweb$inweb$shape = 23
  p2 = plot_overlay(p1, inweb, label = F)
  
  # see whether squares are created
  built = ggplot2::ggplot_build(p2)
  expect_equal(as.character(built$data[[5]]$label), 'BCL2')
  expect_equal(as.character(built$data[[5]]$shape), '22')
  
  # see whether diamonds are created
  expect_true(all(as.character(built$data[[6]]$shape)  == '23'))
  
  # ensure that plotly also reflects these changes
  built2 = make_interactive(p2)
  expect_true(all(as.vector(built2$x$attrs[[2]]$symbols) == c("circle", "circle", "square", "diamond", "diamond")))
  
})


test_that('ggplot sizes are correctly inherited',{
  
  df <- read_input("data/test.data2.txt", sep="\t")$data
  df <- suppressWarnings(calc_mod_ttest(df))
  df <- id_significant_proteins(df)
  p = plot_volcano_basic(df, size_gg = 4, plot_segments = F) %>%
    plot_overlay(as.bait('BCL2'), size_gg = 4.5, stroke = 1, label_size = 10, label_box_padding = 0.3) %>%
    theme_volcano()
  expect_true(!is.null(p))
  
})
  
