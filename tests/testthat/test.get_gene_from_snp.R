context('get_gene_from_snp')


test_that('standard functionality',{
  
  # Known tags in here
  expect_true('BRCA1' %in% unlist(get_gene_from_snp('rs12516')))
  expect_true('LPL' %in% unlist(get_gene_from_snp('rs268')))
  
  # Known tags in here
  res = unlist(get_gene_from_snp('rs268', invert = T))
  expect_true(all(names(res) %in% c("INTS10", "LPL")))
  expect_equal(as.vector(res), rep('rs268', 2))
  
})

