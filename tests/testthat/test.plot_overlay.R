context('plot_overlay')

# for comparing images
#source('functions/compare_image.R')
func = 'plot_overlay'

# read in test data
df <- read_input("data/test.data.txt",header=T,sep="\t")$data
df <- calc_mod_ttest(df)


test_that('simple overlay of a bait',{
  
  # a few different test cases
  id = 'A1'
  df = id_enriched_proteins(df)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p = plot_overlay(p, as.bait('BCL2')) ## testcase here..
  
  #save_gg_reference(p, func, id)
  #expect_true(compare_with_reference(p, func, id))
  
  
  id = 'A2'
  # a few different test cases
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # Generate random dataset
  ref1= data.frame(gene=c('APPL1', 'RAB7A'),col_significant='cyan',col_other='grey')
  ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey')
  reference = list(ref1, ref2)
  names(reference) = c('genes (I)', 'genes (II)')
  
  # overlay second list
  p1 = plot_overlay(p1, reference) # testcase here..
  #expect_true(compare_with_reference(p1, func, id))
  

})

test_that('inweb and gnomad overlay',{
  
  # setup basic test
  id = 'B1'
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # overlay with inweb
  inweb = get_inweb_list('BCL2')
  inweb = list(inweb=inweb[inweb$significant, ])
  p2 = suppressWarnings(plot_overlay(p1, inweb, label = F))
  #expect_true(compare_with_reference(p2, func, id))
  
  # overlay with gnomad
  id = 'B2'
  gnomad = gnomad_table[gnomad_table$gene %in% df$gene & gnomad_table$pLI == 1, ]
  gnomad = list(gnomad = data.frame(gnomad, col_significant = 'blue', col_other = 'grey'))
  p3 = suppressWarnings(plot_overlay(p2, gnomad, label = F))
  #expect_true(compare_with_reference(p3, func, id))
  
})

test_that('invalid columns in overlay gives warning and errrs',{
  
  # invalid columns in overlay gives warning
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  ref1= data.frame(gene=c('APC2', 'RAB7A'),col_significant='cyan',col_other='grey', col_invalid = T)
  ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey', col_invalid = T)
  reference = list(ref1, ref2)
  names(reference) = c('SCZ genelist', 'ASD genelist')
  expect_warning(plot_overlay(p, reference))
  
  # check invalid input 
  expect_error(plot_overlay(p, reference$`SCZ genelist`)) # must be a named list
  
})
