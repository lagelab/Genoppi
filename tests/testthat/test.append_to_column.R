context('append_to_column')

test_that('function fails when no mapping is not provided',{
  
  df = data.frame(gene=c('ALLISON','BOB','MIKKEL'), significant = c(F,T,T), dataset = c('HUMAN','HUMAN','GOLDFISH'))
  df = append_to_column(df)
  expect_equal(df$dataset,c('HUMAN (not significant)','HUMAN (significant)','GOLDFISH (significant)'))
  
})

#test_that('characters are split by \n when nchar_max is specified and collapse_type is trunated',{
  
  #l = paste(rep(letters, 2), collapse = '')
  #L = paste(rep(LETTERS, 2), collapse = '')
  
  #df = data.frame(gene=c('ALLISON','BOB','MIKKEL'), significant = c(F,T,T), dataset = c(l,l,L))
  #df = append_to_column(df, nchar_max = 5)
  #expect_equal(df$dataset,s)
  
#})

data("example_data")

test_that('nchar_max is specified and mapping constraitns are violated',{
  
  df = example_data %>%
    calc_mod_ttest() %>%
    id_significant_proteins()
  
  # unambigious mappoing is not working
  overlay = get_geneset_overlay(df, database = 'bp')
  df1 = df %>% plot_volcano_basic() %>% plot_overlay(overlay, legend_nchar_max = 50) %>% make_interactive()
  expect_true(!is.null(df1))
  
})
