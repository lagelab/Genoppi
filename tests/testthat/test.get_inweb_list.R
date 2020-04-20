context('get_inweb_list')

# BCL2 test case
inwebDf <- data.frame(gene=keys(inweb_hash))
inwebDf$significant <- inwebDf$gene %in% inweb_hash[["BCL2"]]

test_that('get_inweb_list can return correct InWeb data',{

  # InWeb
  result <- get_inweb_list("BCL2")
  expect_equal(result,inwebDf)

  # bait not in InWeb
  expect_true(is.null(get_inweb_list("FAKEBAIT")))

})
