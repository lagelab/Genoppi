context('get_bioplex_list')

test_that('get_bioplex_list can return correct data',{
  
  # all interactions
  res <- get_bioplex_list("TARDBP")
  res_ints <- sort(subset(res,significant)$gene)
  
  ref <- subset(bioplex_table,Gene1=='TARDBP'|Gene2=='TARDBP')
  ref_ints <- sort(setdiff(c(ref$Gene1,ref$Gene2),'TARDBP'))
  
  expect_equal(res_ints,ref_ints)  
  
  # interactions filtered by score cutoff
  res1 <- get_bioplex_list("RRS1", score=0.95)
  res1_ints <- sort(subset(res1,significant)$gene) 
  
  ref1 <- subset(bioplex_table,(Gene1=='RRS1'|Gene2=='RRS1'))
  ref1 <- tidyr::separate_rows(ref1,Score,CellLine,sep=',',convert=T)
  ref1 <- subset(ref1,Score>=0.95)
  ref1_ints <- sort(setdiff(c(ref1$Gene1,ref1$Gene2),'RRS1'))
  
  expect_equal(res1_ints,ref1_ints) 
 
  # bait not found
  expect_true(is.null(get_bioplex_list("FAKEBAIT")))
  
})

test_that('errors are reported correctly',{
  
  expect_error(get_bioplex_list("TARDBP",score='0.5'))
  expect_error(get_bioplex_list("TARDBP",score=2))
  expect_error(get_bioplex_list("TARDBP",socre=-1))
  
})
