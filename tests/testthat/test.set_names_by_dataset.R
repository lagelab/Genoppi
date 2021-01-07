context('set_names_by_dataset')


test_that('colors by dataset and inweb',{
  
  # make dataset
  df <- read_input("data/test.data2.txt", sep="\t")$data
  df <- suppressWarnings(calc_mod_ttest(df))
  df <- id_significant_proteins(df)
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
  global_colors = set_names_by_dataset(list(data, overlay))
  expect_equal(names(global_colors), c("pulldown (significant)", "pulldown (not significant)", "bait (significant)", "inweb (significant)", "inweb (not significant)"))
  expect_equal(as.vector(global_colors), c("#41AB5D", "grey", "red", "yellow", "grey"))
  
  # check errors
  expect_error(set_names_by_dataset(data, overlay))
  expect_error(set_names_by_dataset(data, marker = 'color', by = 2))
  expect_error(set_names_by_dataset(data.frame()))
  expect_error(set_names_by_dataset(list(data.frame(A=c(1,2,3),B=c(1,2,3))), marker = 'C'))
  expect_error(set_names_by_dataset(list(data.frame(A=c(1,2,3),B=c(1,2,3))), by = 'C'))
  
  # check uniqueness error
  overlay$group = 'same group'
  expect_error(set_names_by_dataset(list(overlay)))
  
})



