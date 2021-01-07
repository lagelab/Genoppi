context('get_tissue_lists')

# test HPA Tissue and genes
tissue <- c("blood","esophagus")
genes <- c('ANXA1', 'EEF1G', 'SNCA', 'WARS')
genes_null <- c("TEST1","TEST2","TEST3")




test_that('get_tissue_lists can return correct data.frame',{
  
  # Multiple tissues can be found
  table(hpa_rna$tissue)
  result <- get_tissue_lists('skin 1',hpa_rna)
  expect_equal(result, hpa_rna[hpa_rna$tissue == 'skin 1',])
  
})

test_that('errors are prompted',{
  
  expect_error(get_tissue_lists(3,hpa_rna))
  expect_error(get_tissue_lists('myheart',hpa_rna))

})


