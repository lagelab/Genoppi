context('collapse_labels')

# read in test data
df <- read_input("data/test.data.txt",header=T,sep="\t")$data
df <- suppressWarnings(calc_mod_ttest(df))

test_that('get colors by artifical dataset',{

  # setup basic plot
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # overlay with inweb
  inweb = get_inweb_list('BCL2')
  inweb = list(inweb=inweb[inweb$significant, ])
  p2 = suppressWarnings(plot_overlay(p1, inweb, label = F))
  
  # fake overlay
  myoverlay = list(myoverlay=head(data.frame(inweb$inweb, col_significant = 'brown', col_other = 'brown')))
  p3 = suppressWarnings(plot_overlay(p2, myoverlay, label = F))
  
  # Expect only two of myoverlay to be present in data
  expect_equal(sum(myoverlay$myoverlay$gene %in% df$gene), 2)
  
  # expect two rows of myoverlay and inweb to be collapsed
  collapsed = collapse_labels(p3$overlay, collapse_into = 'mycol', dataset_collapse_sep = '', item_sep = '-')
  
  # collapsed by gene, so only one gene should be present
  expect_true(all(table(collapsed$gene) == 1))
  
  # the two genes to be present
  expect_equal(sum(myoverlay$myoverlay$gene %in% collapsed$gene), 2)

  # check that the two labels were merged
  expect_equal(collapsed[collapsed$gene %in% myoverlay$myoverlay$gene,]$mycol,
               c("inweb-myoverlay","inweb-myoverlay"))
  
  # make checks for colors

})








