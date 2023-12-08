context('get_ppi_list')

test_that('get_ppi_list can return correct data',{

  # BioPlex
  res <- get_ppi_list("TARDBP",'bioplex')
  res_ints <- sort(subset(res,significant)$gene)
  ref <- subset(bioplex_table,Gene1=='TARDBP'|Gene2=='TARDBP')
  ref_ints <- sort(setdiff(c(ref$Gene1,ref$Gene2),'TARDBP'))  
  expect_equal(res_ints,ref_ints)

  # InWeb
  res1 <- get_ppi_list("TARDBP",'inweb')
  res1_ints <- sort(subset(res1,significant)$gene)
  ref1 <- subset(inweb_table,Gene1=='TARDBP'|Gene2=='TARDBP')
  ref1_ints <- sort(setdiff(c(ref1$Gene1,ref1$Gene2),'TARDBP'))  
  expect_equal(res1_ints,ref1_ints)
  
  # iRefIndex
  res2 <- get_ppi_list("TARDBP",'irefindex')
  res2_ints <- sort(subset(res2,significant)$gene)
  ref2 <- subset(irefindex_table,Gene1=='TARDBP'|Gene2=='TARDBP')
  ref2_ints <- sort(setdiff(c(ref2$Gene1,ref2$Gene2),'TARDBP'))  
  expect_equal(res2_ints,ref2_ints)

  # STRING
  res3 <- get_ppi_list("TARDBP",'string')
  res3_ints <- sort(subset(res3,significant)$gene)
  ref3 <- subset(string_table,Gene1=='TARDBP'|Gene2=='TARDBP')
  ref3_ints <- sort(setdiff(c(ref3$Gene1,ref3$Gene2),'TARDBP'))  
  expect_equal(res3_ints,ref3_ints)

  # bait not found
  expect_true(is.null(get_ppi_list("FAKEBAIT",'inweb')))

})

test_that('errors are reported correctly',{
  
  expect_error(get_ppi_list('TARDBP','FAKEDATASET')) 
  
})
