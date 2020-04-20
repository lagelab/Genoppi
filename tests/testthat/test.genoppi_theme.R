context('genoppi_theme')

data("example_data")

test_that('basic test to see if function works',{
  
  # perform moderated t-test
  stats_df <- calc_mod_ttest(example_data)
  
  # identify enriched proteins
  sig_df <- id_enriched_proteins(stats_df)
  
  # generate volcano plot with bait protein labeled
  basic_volcano <- plot_volcano_basic(sig_df)
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2')) + theme_genoppi()
  expect_true(!is.null(basic_volcano$theme$panel.background))
  expect_true(!is.null(basic_volcano$theme$panel.grid.major))
  expect_true(!is.null(basic_volcano$theme$panel.grid.minor))
  
  
  # test rotate (expect no errors)
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2')) + theme_genoppi_bar()
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2')) + theme_genoppi_bar(rotate=F)
  
})