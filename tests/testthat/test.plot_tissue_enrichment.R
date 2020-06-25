context('plot_tissue_enrichment')

data("GTEX_table")
data("example_data")
stats_df <- calc_mod_ttest(example_data)
sig_df <- id_enriched_proteins(stats_df)
gtex_enrichment = calc_adjusted_enrichment(sig_df, GTEX_table, bait = 'BCL2')
  
test_that('plot_tissue_enrichemnt', {
  
  barplot = plot_tissue_enrichment(data = gtex_enrichment, 
                           col.tissue = 'list_name', 
                           col.value = 'pvalue')
  
  expect_true(!is.null(barplot))
  
})


test_that('plotly_tissue_enrichemnt', {
  
  # setup basic plot
  barplot = plotly_tissue_enrichment(data = gtex_enrichment, 
                                     col.tissue = 'list_name', 
                                     col.value = 'pvalue', 
                                     col.value.text = 'pvalue',
                                     col.value.order = 'increasing')
  
  # tests
  expect_true(all(barplot$x$visdat[[1]]()$list_name %in% gtex_enrichment$list_name))
  
  
})






