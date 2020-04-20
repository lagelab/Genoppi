context('venn_to_table')


test_that('basic functionality',{
  
  
  # can delinate lists
  venn = list(A=c(1,2,3),
              B=c(2,3,4))
  
  
  res = venn_to_table(venn)
  ref = data.frame(A=as.character(c(1, 2, 3, NA)),
                   B=as.character(c(NA, 2, 3, 4)),
                   overlap=c(0, 1, 1, 0),
                   stringsAsFactors = F)
  
  expect_equal(res, ref)
  
  # error when invalid input.. this
  # should be fixed in the future to take
  # multiple lists
  venn = list(A=c(1,2,3),
              B=c(2,3,4),
              c=c(4,5,6))
  expect_error(venn_to_table(venn))
  
  
})
