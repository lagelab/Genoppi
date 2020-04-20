context('make_interactive')

data("example_data")

test_that('basic test to see if function works',{
  
  # perform moderated t-test
  stats_df <- calc_mod_ttest(example_data)
  
  # identify enriched proteins
  sig_df <- id_enriched_proteins(stats_df)
  
  # generate volcano plot with bait protein labeled
  basic_volcano <- plot_volcano_basic(sig_df)
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2'))
  
  # interactive volcano plot
  result = make_interactive(bait_volcano) %>%
    add_layout_html_axes_volcano(width = NULL, height = NULL)
  expect_true(!is.null(result))
  
  # look at nnotations
  m = result$x$layoutAttrs[[1]]$annotations
  expect_equal(quo_name(m$x),'logFC')
  expect_equal(quo_name(m$y),'-log10(pvalue)')
  expect_equal(quo_name(m$text),'BCL2')
  
})


