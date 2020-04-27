context('calc_logfc_limits')

set.seed(1)

test_that('logfc direction is positive',{
  
  
  df <- data.frame(accession_number=c('Q96DR7', 'Q13148', 'P17948'), rep1=rnorm(3), rep2=rnorm(3))
  df <- map_gene_id(df) %>% calc_mod_ttest()
  res <- calc_logfc_limit(df,logfc_direction = 'positive')
  expect_equal(res, round(max(df$logFC)+0.5, 1))

})

test_that('logfc direction is negative',{
  
  df <- data.frame(accession_number=c('Q96DR7', 'Q13148', 'P17948'), rep1=rnorm(3), rep2=rnorm(3))
  df <- map_gene_id(df) %>% calc_mod_ttest()
  res <- calc_logfc_limit(df,logfc_direction = 'negative')
  expect_equal(res, round(abs(min(df$logFC))+0.5, 1))
  
})