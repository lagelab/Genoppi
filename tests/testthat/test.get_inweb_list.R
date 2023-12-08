context('get_inweb_list')

test_that('get_inweb_list can return correct data',{

  # all interactions
  res <- get_inweb_list("TARDBP")
  res_ints <- sort(subset(res,significant)$gene)

  ref <- subset(inweb_table,Gene1=='TARDBP'|Gene2=='TARDBP')
  ref_ints <- sort(setdiff(c(ref$Gene1,ref$Gene2),'TARDBP'))

  expect_equal(res_ints,ref_ints)

  # high-confidence interactions
  result2 <- get_inweb_list("TARDBP",type="hc")
  expect_equal(sum(result2$significant),345)

  # gold-standard interactions
  result3 <- get_inweb_list("TARDBP",type="gs")
  expect_equal(sum(result3$significant),25)

  # bait not in InWeb
  expect_true(is.null(get_inweb_list("FAKEBAIT")))

})

test_that('errors are reported correctly',{

  expect_error(get_inweb_list("TARDBP",type="FAKETYPE"))

})
