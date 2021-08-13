context('get_geneset_overlay')

# get data
data("example_data")
df = example_data %>% 
  calc_mod_ttest(two_sample=F) %>% 
  id_significant_proteins()

# get all genesets
genes_sig = df$gene[df$significant]
genesets_sig = assign_freq(get_pathways('hgnc', genes_sig), 'pathway')

test_that('all genes are found when k > Inf',{
  
  # all pathways are captured when k >> n
  overlay1 = get_geneset_overlay(df, 'hgnc', k = +Inf)$geneset
  expect_true(all(overlay1$gene %in% genesets_sig$gene))
  expect_equal(
    table(overlay1$gene, overlay1$dataset),
    table(genesets_sig$gene, genesets_sig$pathway)
  )
  
})

test_that('pathways are correctly subsetted by recurrent genests',{
  
  # k = 1
  x = suppressWarnings(get_geneset_overlay(df, 'hgnc', k = 1))
  expect_lte(lun(x$geneset$pathway),1) # less than or equal to
  expect_equal(nrow(x$geneset), 7)
  
  # k = 2
  x = get_geneset_overlay(df, 'hgnc', k = 2)
  expect_lte(lun(x$geneset$pathway), 2)
  expect_equal(nrow(x$geneset), 10)
  
  # k = 10
  x = get_geneset_overlay(df, 'hgnc', k = 10)
  expect_lte(lun(x$geneset$pathway), 10)
  expect_equal(nrow(x$geneset), 18) 
 
})

test_that('k is NULL will result in NO subset',{

  x1 = get_geneset_overlay(df, 'hgnc', k = +Inf)$geneset
  x2 = get_geneset_overlay(df, 'hgnc', k = NULL)$geneset
  expect_equal(x1, x2)

})


test_that('common fuction call', {
 
  plot_volcano_basic(df) %>% 
    plot_overlay(get_geneset_overlay(df, 'hgnc', k = 3)) %>% 
    make_interactive()
  
  plot_volcano_basic(df) %>% 
    plot_overlay(get_geneset_overlay(df, 'hgnc', k = 10)) %>% 
    make_interactive()
  
  plot_volcano_basic(df) %>% 
      plot_overlay(get_geneset_overlay(df, 'hgnc', k = 25)) %>% 
      make_interactive()
  
  expect_true(TRUE) # exepect function to get here without error
    
})

