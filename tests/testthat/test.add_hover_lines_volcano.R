context('add_hover_lines_volcano')

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
    add_hover_lines_volcano(sig_type = 'pvalue',line_pvalue = 0.05,line_logfc = 5)
  
  # check that  pvalyelines are plotted
  m1 = result$x$attrs[[5]]
  expect_true(!is.null(m1$line))
  expect_equal(m1$mode, 'lines')
  expect_equal(m1$text, "pvalue = 0.05")
  
  # check that logfc lines are plotted
  m2 = result$x$attrs[[6]]
  expect_true(!is.null(m2$line))
  expect_equal(m2$mode, 'lines')
  expect_equal(m2$text, "logFC = -5")

  
})