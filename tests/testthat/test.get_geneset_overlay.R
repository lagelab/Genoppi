context('get_geneset_overlay')

# get data
data("example_data")
df = example_data %>% 
  calc_mod_ttest() %>% 
  id_enriched_proteins()

# get all genesets
#genes_sig = df$gene[df$significant]
#genesets_sig = assign_freq(get_pathways('hgnc', genes_sig), 'pathway')

test_that('basisc functionality',{
  
  
  # all pathways are captured when k >> n
  #overlay1 = get_geneset_overlay(df, 'hgnc', k = +Inf)$geneset
  #expect_equal(overlay1$gene, genesets_sig$gene)
  #expect_equal(
  #  table(overlay1$gene, overlay1$dataset),
  #  table(genesets_sig$gene, genesets_sig$pathway)
  #)
  
  
  
})

test_that('pathways are correctly subsetted by recurrent genests',{
  
  #overlay2 = get_geneset_overlay(df, 'hgnc', k = 5)$geneset
  
  #counts = data.frame(pathway=genesets_sig$pathway, Freq=genesets_sig$Freq)
  #counts = counts[!duplicated(counts),]
  
  #revcumsum = cumsum(rev(table(counts$Freq)))
  #lowest_allowed_freq = as.numeric(names(rev(revcumsum[revcumsum < 5]))[1])
  
  
  
  # test
  #expected_dataset = genesets_sig[genesets_sig$Freq >= lowest_allowed_freq,]
  #overlay2
  #expected_dataset
  
  #nrow(expected_dataset)
  #nrow(overlay2)
  
  #expect_equal(
  #  overlay2$dataset,
  #  expected_dataset
  #)

  
  

  
  #overlay1 = get_geneset_overlay(df, 'hgnc', k = 25)$geneset
  
  
  
})








