context('get_irefindex_list')

test_that('get_irefindex_list can return correct Bioplex data',{
  
  # all interactions
  #res <- get_irefindex_list("TARDBP")
  #ref <- irefindex_table[(irefindex_table$Gene1 == 'TARDBP' | irefindex_table$Gene2 == 'TARDBP') & irefindex_table$Score.np.max >= 1,]
  
  
  #expect_equal(nrow(reference), sum(result$significant))
  
  # check that p works for subsetting
  #result1 <- get_bioplex_list("RRS1", p = 0.5)
  #reference1 <- irefindex_table[(irefindex_table$Gene1 == 'RRS1' | irefindex_table$Gene2 == 'RRS1') & irefindex_table$Score.np.max >= 1,]
  #expect_equal(nrow(reference1), sum(result1$significant))
  #expect_equal(nrow(reference1), 138)
  
  #result2 <- get_bioplex_list("RRS1", p = 0.95)
  #reference2 <- irefindex_table[(irefindex_table$Gene1 == 'RRS1' | irefindex_table$Gene2 == 'RRS1') & irefindex_table$Score.np.max >= 300,]
  #expect_equal(nrow(reference2), sum(result2$significant))
  #expect_equal(nrow(reference2), result2, 47)
  
  # bait not found
  #expect_true(is.null(get_bioplex_list("FAKEBAIT")))
  
})

test_that('errors are reported correctly',{
  
  expect_error(get_bioplex_list("TARDBP",n = '4'))
  expect_error(get_bioplex_list("TARDBP",n = 0))
  expect_error(get_bioplex_list("TARDBP",n = -1))
  
})
