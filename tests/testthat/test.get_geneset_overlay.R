context('get_geneset_overlay')

# get data
data("example_data")
df = example_data %>% 
  calc_mod_ttest() %>% 
  id_enriched_proteins()

# get all genesets
genes_sig = df$gene[df$significant]
genesets_sig = assign_freq(get_pathways('hgnc', genes_sig), 'pathway')

test_that('all genes are found when k > Inf',{
  
  # all pathways are captured when k >> n
  overlay1 = get_geneset_overlay(df, 'hgnc', k = +Inf)$geneset
  expect_equal(overlay1$gene, genesets_sig$gene)
  expect_equal(
    table(overlay1$gene, overlay1$dataset),
    table(genesets_sig$gene, genesets_sig$pathway)
  )
  
})

test_that('pathways are correctly subsetted by recurrent genests',{
  
  # k = 1
  x = suppressWarnings(get_geneset_overlay(df, 'hgnc', k = 1))
  expect_equal(nrow(x$geneset), 0)
  
  # k = 2
  x = get_geneset_overlay(df, 'hgnc', k = 2)
  expect_equal(nrow(x$geneset), 8)
  expect_equal(lun(x$geneset$pathway), 2)
  
  # k = 3
  x = get_geneset_overlay(df, 'hgnc', k = 3)
  expect_equal(lun(x$geneset$pathway), 2)
  
  # k = 4
  x = get_geneset_overlay(df, 'hgnc', k = 4)
  expect_equal(lun(x$geneset$pathway), 4)
  
})








