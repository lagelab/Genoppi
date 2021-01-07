context('add_plotly_markers_search')

data("example_data")

test_that('basic test to see if function works',{
  
  # perform moderated t-test
  stats_df <- calc_mod_ttest(example_data)
  sig_df <- id_significant_proteins(stats_df)
  
  # generate volcano plot with bait protein labeled
  basic_volcano <- plot_volcano_basic(sig_df)
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2'))
  
  # interactive volcano plot
  result = make_interactive(bait_volcano) %>%
    add_plotly_markers_search('DD')
  
  # check result
  m = result$x$attrs[[5]]
  expect_true(!is.null(m))
  expect_equal(m$hoverinfo, "text+x+y")
  expect_equal(quo_name(m$x), 'logFC')
  expect_equal(quo_name(m$y), "-log10(pvalue)")
  
})
