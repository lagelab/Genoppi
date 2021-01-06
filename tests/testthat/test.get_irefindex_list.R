context('get_irefindex_list')

test_that('get_irefindex_list can return correct Bioplex data',{
  
  # all interactions
  res <- get_irefindex_list("TARDBP")
  ref <- irefindex_table[(irefindex_table$Gene1 == 'TARDBP' | irefindex_table$Gene2 == 'TARDBP') & irefindex_table$Score.np.max >= 1,]
  res_genes <- unique(res$gene[res$significant])
  ref_genes <- unique(c(ref$Gene1, ref$Gene2))
  expect_equal(length(ref_genes)-1, length(res_genes)) # - 1 for self interacting TARFBP
  
  # bait not found
  expect_true(is.null(get_irefindex_list("FAKEBAIT")))
  
})

test_that('errors are reported correctly',{
  
  expect_error(get_irefindex_list("TARDBP",n = '4'))
  expect_error(get_irefindex_list("TARDBP",n = 0))
  expect_error(get_irefindex_list("TARDBP",n = -1))
  
})
