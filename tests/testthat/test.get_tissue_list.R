context('get_tissue_list')

test_that('get_tissue_lists can return correct data.frame',{
  
  # Multiple tissues can be found
  table(hpa_rna_table$tissue)
  result <- get_tissue_list('skin 1',hpa_rna_table)
  expect_equal(result, hpa_rna_table[hpa_rna_table$tissue == 'skin 1',])
  
})

test_that('errors are prompted',{
  
  expect_error(get_tissue_list(3,hpa_rna_table))
  expect_error(get_tissue_list('myheart',hpa_rna_table))

})

