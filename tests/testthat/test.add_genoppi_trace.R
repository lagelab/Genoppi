context('add_genoppi_trace')

data("example_data")

test_that('basic test to see if function works',{
  
  # perform moderated t-test
  stats_df <- calc_mod_ttest(example_data)
  
  # identify enriched proteins
  sig_df <- id_enriched_proteins(stats_df)
  
  # generate volcano plot with bait protein labeled
  p <- plot_volcano_basic(sig_df)
  p <- plot_overlay(p, as.bait('BCL2'))
  
  # ensure data and overlay has appropiate columns for plotting
  data = to_overlay_data(append_to_column(p$data)) 
  overlay = to_overlay_data(append_to_column(p$overlay, sig_text = 'sig_text'))
  overlay$color = ifelse(overlay$significant, as.character(overlay$col_significant), as.character(overlay$col_other))
  
  # get the global symbol and color mapping and save in local environemnt
  sizes = c(min(c(p$data$size, p$overlay$size)), max(c(p$data$size, p$overlay$size)))
  global_colors = set_names_by_dataset(data, overlay, marker = 'color') 
  global_symbols = set_names_by_dataset(data, overlay, marker= 'symbol') 
  ggparams = p
  params = environment()
  
  # interactive volcano plot
  result = plot_ly(source = source, sizes = sizes) %>%  # sizes=c(7,29)
    add_genoppi_trace(data[data$gene %nin% overlay$gene,], params)
  
  # look at nnotations
  m = result$x$attrs[[2]]
  expect_true(!is.null(result$x))
  expect_equal(quo_name(m$x),'logFC')
  expect_equal(quo_name(m$y),'-log10(pvalue)')
  expect_equal(quo_name(m$text),'~gene')
  
})


