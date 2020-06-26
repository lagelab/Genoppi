context('calc_adjusted_enrichment')

data("example_data")
data(gtex_table)
data(example_data)

# run data
data = example_data %>% calc_mod_ttest %>% id_enriched_proteins()


test_that("Basic functionality GTEX", {
  
  # exepcted p values and expected calc_hypers counts
  gtex_enrichment = calc_adjusted_enrichment(data, gtex_table, bait = 'BCL2')
  expect_equal(sum(gtex_enrichment$pvalue), 31.54759, tolerance = 10e-5)
  expect_equal(sum(gtex_enrichment$successInSample_count), 253)
  expect_equal(mean(gtex_enrichment$BH.FDR), 0.991033, tolerance = 10e-5)
  
})

test_that("errors are prompted correctly", {
  
  data1 <- data
  data1$gene <- NULL
  expect_error(calc_adjusted_enrichment(data1, gtex_table, bait = 'BCL2'))
  
  gtex_table1 = gtex_table
  gtex_table1$significant <- NULL
  expect_error(calc_adjusted_enrichment(data, gtex_table1, bait = 'BCL2'))
  
})


