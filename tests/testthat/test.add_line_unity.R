context('add_line_unity')

data("example_data")

test_that('basic',{
  
  # perform moderated t-test
  stats_df <- calc_mod_ttest(example_data)
  sig_df <- id_enriched_proteins(stats_df)
  
  # generate volcano plot with bait protein labeled
  basic_scatter <- plot_scatter_basic(sig_df)
  bait_scatter <- plot_overlay(basic_scatter,as.bait('BCL2'))
  
  # interactive volcano plot
  result = make_interactive(bait_scatter) %>%
    add_line_unity() 
  
  expect_true(!is.null(result$x))
})