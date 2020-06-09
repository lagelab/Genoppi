context('venn_to_table')


test_that('basic functionality',{
  
  
  # overlapping lists
  venn = list(A=c(1,2,3),
              B=c(2,3,4))
  
  
  res = venn_to_table(venn)
  ref = data.frame(gene = as.character(c(2, 3, 1, 4)), dataset = c('AB', 'AB', 'A', 'B'), stringsAsFactors = F)
  expect_equal(res, ref)
  
  # lists that do not overlap
  venn = list(A=c(1,2,3),
              B=c(4,5,6))
  
  
  res = venn_to_table(venn)
  ref = data.frame(gene = as.character(c(1, 2, 3, 4, 5, 6)), dataset = c('A', 'A', 'A', 'B', 'B', 'B'), stringsAsFactors = F)
  expect_equal(res, ref)
  
  
  # error when invalid input.. this
  # should be fixed in the future to take
  # multiple lists
  venn = list(A=c(1,2,3),
              B=c(2,3,4),
              c=c(4,5,6))
  expect_error(venn_to_table(venn))
  
  
})
