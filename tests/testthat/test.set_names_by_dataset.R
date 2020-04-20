context('set_names_by_dataset')


test_that('colors by dataset and inweb',{
  
  # make dataset
  df <- read_input("data/test.data.txt",header=T,sep="\t")$data
  df <- calc_mod_ttest(df)
  df <- id_enriched_proteins(df)
  df$dataset = 'pulldown'
  
  # first plot
  p = plot_volcano_basic(df)
  p1 = plot_overlay(p, as.bait('BCL2'), label = F)
  
  # get overlay dataset
  inweb = get_inweb_list('BCL2')
  inweb = list(inweb=inweb[inweb$significant, ])
  p2 = suppressWarnings(plot_overlay(p1, inweb, label = F))
  
  # standardize data
  data = to_overlay_data(append_to_column(p2$data)) 
  overlay = to_overlay_data(append_to_column(p2$overlay))
  overlay$color = ifelse(overlay$significant, as.character(overlay$col_significant), as.character(overlay$col_other))
  
  # check function
  global_colors = set_names_by_dataset(data, overlay)
  expect_equal(names(global_colors), c("pulldown (enriched)", "pulldown (not enriched)", "bait (not enriched)", "inweb (not enriched)", "inweb (enriched)"))
  expect_equal(as.vector(global_colors), c("#41AB5D", "grey", "orange", "grey", "yellow"))
  
})