context('get_bioplex_list')

test_that('get_bioplex_list can return correct Bioplex data',{
  
  # all interactions
  result <- get_bioplex_list("TARDBP", p = 0.5)
  reference <- bioplex_table[(bioplex_table$Gene1 == 'TARDBP' | bioplex_table$Gene2 == 'TARDBP') & bioplex_table$pInt >= 0.5,]
  expect_equal(reference$Gene2, result$gene[result$significant])
  
  # check that p works for subsetting
  result1 <- get_bioplex_list("RRS1", p = 0.5)
  reference1 <- bioplex_table[(bioplex_table$Gene1 == 'RRS1' | bioplex_table$Gene2 == 'RRS1') & bioplex_table$pInt >= 0.5,]
  expect_equal(nrow(reference1), sum(result1$significant))
  expect_equal(nrow(reference1), 138)
  
  result2 <- get_bioplex_list("RRS1", p = 0.95)
  reference2 <- bioplex_table[(bioplex_table$Gene1 == 'RRS1' | bioplex_table$Gene2 == 'RRS1') & bioplex_table$pInt >= 0.95,]
  expect_equal(nrow(reference2), sum(result2$significant))
  expect_equal(nrow(reference2), 47)
  
  # bait not found
  expect_true(is.null(get_bioplex_list("FAKEBAIT")))
  
})

test_that('errors are reported correctly',{
  
  expect_error(get_bioplex_list("TARDBP",p = '0.5'))
  expect_error(get_bioplex_list("TARDBP",p = 2))
  expect_error(get_bioplex_list("TARDBP",p = -1))
  
})
