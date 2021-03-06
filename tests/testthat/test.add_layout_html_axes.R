context('add_plotly_legend_volcano')

data("example_data")

test_that('add layout to volcano',{
  
  # perform moderated t-test
  stats_df <- calc_mod_ttest(example_data)
  sig_df <- id_significant_proteins(stats_df)
  
  # generate volcano plot with bait protein labeled
  basic_volcano <- plot_volcano_basic(sig_df)
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2'))
  
  # interactive volcano plot
  result = make_interactive(bait_volcano) %>%
    add_plotly_layout_volcano()
  
  # check result
  m = result$x$layoutAttrs[[2]]
  expect_true(!is.null(m))
  expect_equal(m$xaxis$title, "log<sub>2</sub>(Fold change)")
  expect_equal(m$yaxis$title, "-log<sub>10</sub>(<i>P</i>-value)")
  
})
