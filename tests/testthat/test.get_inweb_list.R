context('get_inweb_list')

test_that('get_inweb_list can return correct InWeb data',{

  # all interactions
  result <- get_inweb_list("TARDBP",type="all")
  expect_equal(nrow(result),18813)
  expect_equal(sum(result$significant),464)

  # high-confidence interactions
  result2 <- get_inweb_list("TARDBP",type="hc")
  expect_equal(sum(result2$significant),350)

  # gold-standard interactions
  result3 <- get_inweb_list("TARDBP",type="gs")
  expect_equal(sum(result3$significant),26)

  # bait not in InWeb
  expect_true(is.null(get_inweb_list("FAKEBAIT")))

})

test_that('errors are reported correctly',{

  expect_error(get_inweb_list("TARDBP",type="FAKETYPE"))

})
