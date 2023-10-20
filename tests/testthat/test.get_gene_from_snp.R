context('get_gene_from_snp')

test_that('standard functionality',{
  
  # Known tags in here
  expect_true('BRCA1' %in% unlist(get_gene_from_snp('rs12516')))
  expect_true('ATXN2' %in% unlist(get_gene_from_snp('rs848132')))
  
  # Known tags in here
  res <- unlist(get_gene_from_snp('rs848132', invert = T))
  expected_genes <- c('ACAD10','ALDH2','ATXN2','BRAP','CUX2','PHETA1','SH2B3')
  expect_true(all(names(res) %in% expected_genes))
  expect_equal(as.vector(res), rep('rs848132', length(expected_genes)))
  
})

