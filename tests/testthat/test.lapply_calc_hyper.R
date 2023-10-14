context('lapply_calc_hyper')

data(gtex_rna_table)
data(example_data)

# run data
data = example_data %>% calc_mod_ttest(two_sample=F) %>% id_significant_proteins()


test_that("Basic functionality GTEX", {
  
  # exepcted p values and expected calc_hypers counts
  gtex_enrichment = lapply_calc_hyper(data, gtex_rna_table, bait = 'BCL2')
  
  # need to update after changing gtex_rna_table
  #expect_equal(sum(gtex_enrichment$pvalue), 29.99958, tolerance = 10e-5)
  #expect_equal(sum(gtex_enrichment$successInSample_count), 231)
  #expect_equal(mean(gtex_enrichment$BH.FDR), 0.7670045, tolerance = 10e-5)
  
})

test_that("errors are prompted correctly", {
  
  data1 <- data
  data1$gene <- NULL
  expect_error(lapply_calc_hyper(data1, gtex_rna_table, bait = 'BCL2'))
  
  gtex_table1 = gtex_rna_table
  gtex_table1$significant <- NULL
  expect_error(lapply_calc_hyper(data, gtex_table1, bait = 'BCL2'))
  
})


