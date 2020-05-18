context('parse_uploaded_file')

# make artificial data
df1 <- data.frame(gene=letters, rep1=rnorm(26), rep2=rnorm(26), rep3=rnorm(26))
tmp <- tempfile()
write.table(df1, tmp, quote=F, sep="\t", row.names=F)

test_that('standard functionality',{
  
  # check returned data.frame 
  res <- parse_uploaded_file(tmp)
  ref = df1 %>% calc_mod_ttest()
  expect_equal(res, ref)
  
})

test_that('errors are thrown',{
  
  # check returned data.frame 
  expect_error(parse_uploaded_file('no_such_file.tsv'))
  expect_error(parse_uploaded_file(c(tmp, tmp)))
  
})


