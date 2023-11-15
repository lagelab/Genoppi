context('theme_genoppi')

data("example_data")

# perform moderated t-test
stats_df <- calc_mod_ttest(example_data)

# identify enriched proteins
sig_df <- id_significant_proteins(stats_df)
test_that('basic test to see if function works',{
  
  # generate volcano plot with bait protein labeled
  basic_volcano <- plot_volcano_basic(sig_df)
  bait_volcano <- theme_volcano_custom(plot_overlay(basic_volcano,as.bait('BCL2')))
  expect_true(!is.null(basic_volcano$theme$panel.background))
  expect_true(!is.null(basic_volcano$theme$panel.grid.major))
  expect_true(!is.null(basic_volcano$theme$panel.grid.minor))
  
  # test rotate (expect no errors)
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2')) + theme_genoppi_bar()
  expect_true(!is.null(bait_volcano$theme))
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2')) + genoppi:::theme_genoppi_bar(rotate=F)
  expect_true(!is.null(bait_volcano$theme))
  
})

test_that('aesthethic themes work',{
  
  # volcano
  basic_volcano <- plot_volcano_basic(sig_df)
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2'))
  bait_volcano_theme <- theme_volcano(bait_volcano)
  expect_true(!is.null(bait_volcano_theme))
  
  # scatter
  basic_scatter <- plot_scatter_basic(sig_df)
  bait_scatter <- plot_overlay(basic_scatter,as.bait('BCL2'))
  bait_scatter_theme <- theme_scatter(bait_scatter)
  expect_true(!is.null(bait_volcano_theme))
  
})


