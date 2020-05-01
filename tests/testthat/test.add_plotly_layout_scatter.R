context('add_plotly_legend_scatter')

data("example_data")

test_that('add layout to scatter plot',{
  
  # perform moderated t-test
  stats_df <- calc_mod_ttest(example_data)
  sig_df <- id_enriched_proteins(stats_df)
  
  # generate volcano plot with bait protein labeled
  basic_scatter <- plot_scatter_basic(sig_df)
  bait_scatter <- plot_overlay(basic_scatter,as.bait('BCL2'))
  
  # interactive volcano plot
  result = make_interactive(bait_scatter) %>%
    add_plotly_layout_scatter()
  
  # check result
  m = result$x$layoutAttrs[[2]]
  expect_true(!is.null(m))
  expect_equal(m$xaxis$title, "Replicate 1 log<sub>2</sub>(Fold change)")
  expect_equal(m$yaxis$title, "Replicate 2 log<sub>2</sub>(Fold change)")
  
})