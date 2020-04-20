context('append_to_column')

test_that('function fails when no mapping is not provided',{
  
  df = data.table::data.table(gene=c('ALLISON','BOB','MIKKEL'), significant = c(F,T,T), dataset = c('HUMAN','HUMAN','GOLDFISH'))
  df = append_to_column(df)
  expect_equal(df$dataset,c('HUMAN (not enriched)','HUMAN (enriched)','GOLDFISH (enriched)'))
  
})
