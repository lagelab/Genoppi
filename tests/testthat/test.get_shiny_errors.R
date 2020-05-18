context('get_shiny_errors')

df = data.frame(gene=letters, rep1=runif(26), rep2=runif(26))

test_that('replicates are numeric and sufficient input of column names are valid',{
  
  # no errors
  expect_true(get_shiny_errors(df) == '')
  
  # non-numeric rep columns
  df1 = df
  df1$rep2[6] <- 'blabla'
  expect_true(grepl('numeric' ,get_shiny_errors(df1)))
  
  # invalid column names
  df2 = df
  colnames(df2) <- letters[1:3]
  expect_true(grepl('recognized' ,get_shiny_errors(df2)))
  
  df3 <- df
  df3$accession_number <- runif(26)
  expect_true(grepl('accession' ,get_shiny_errors(df3)))
  
  df4 <- df
  df4$gene <- runif(26)
  expect_true(grepl('gene' ,get_shiny_errors(df4)))
  
  df5 <- df
  df5$logFC <- runif(26)
  df5$logFC[4] <- 'hello'
  expect_true(grepl('logFC' ,get_shiny_errors(df5)))
  
  df6 <- df
  df6$FDR <- runif(26)
  df6$FDR[4] <- 'hello'
  expect_true(grepl('FDR' ,get_shiny_errors(df6)))
  
  df7 <- df
  df7$pvalue <- runif(26)
  df7$pvalue[4] <- 'hello'
  expect_true(grepl('pvalue' ,get_shiny_errors(df7)))
  
})
