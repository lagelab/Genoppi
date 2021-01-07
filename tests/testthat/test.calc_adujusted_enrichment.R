context('calc_adjusted_enrichment')

data(gtex_rna)
data(example_data)

# run data
data = example_data %>% calc_mod_ttest %>% id_significant_proteins()


test_that("Basic functionality GTEX", {
  
  # exepcted p values and expected calc_hypers counts
  gtex_enrichment = calc_adjusted_enrichment(data, gtex_rna, bait = 'BCL2')
  expect_equal(sum(gtex_enrichment$pvalue), 29.51632, tolerance = 10e-5)
  expect_equal(sum(gtex_enrichment$successInSample_count), 245)
  expect_equal(mean(gtex_enrichment$BH.FDR), 0.7670999, tolerance = 10e-5)
  
})

test_that("errors are prompted correctly", {
  
  data1 <- data
  data1$gene <- NULL
  expect_error(calc_adjusted_enrichment(data1, gtex_rna, bait = 'BCL2'))
  
  gtex_table1 = gtex_rna
  gtex_table1$significant <- NULL
  expect_error(calc_adjusted_enrichment(data, gtex_table1, bait = 'BCL2'))
  
})


