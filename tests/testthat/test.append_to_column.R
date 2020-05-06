context('append_to_column')

test_that('function fails when no mapping is not provided',{
  
  df = data.frame(gene=c('ALLISON','BOB','MIKKEL'), significant = c(F,T,T), dataset = c('HUMAN','HUMAN','GOLDFISH'))
  df = append_to_column(df)
  expect_equal(df$dataset,c('HUMAN (not enriched)','HUMAN (enriched)','GOLDFISH (enriched)'))
  
})

test_that('characters are cut off in group when nchar_max is specified.',{
  
  l = paste(rep(letters, 5), collapse = '')
  L = paste(rep(LETTERS, 5), collapse = '')
  
  df = data.frame(gene=c('ALLISON','BOB','MIKKEL'), significant = c(F,T,T), dataset = c(l,l,L))
  df = append_to_column(df, nchar_max = 5)
  expect_equal(df$dataset,c('abcde...','abcde...','ABCDE...'))
  
})
