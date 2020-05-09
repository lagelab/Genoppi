context('collapse_labels')

# read in test data
df <- read_input("data/test.data.txt", sep="\t")$data
df <- suppressWarnings(calc_mod_ttest(df))

test_that('basic functionality, i.e. columns are collapsed as expeced',{

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
  expect_true(all(table(as.character(collapsed$gene)) == 1))
  
  # the two genes to be present
  expect_equal(sum(myoverlay$myoverlay$gene %in% collapsed$gene), 2)

  # check that the two labels were merged
  expect_equal(collapsed[collapsed$gene %in% myoverlay$myoverlay$gene,]$mycol,
               c("inweb-myoverlay","inweb-myoverlay"))
  
  # make checks for colors

})

test_that('NAs in alt label are discarded',{
  
  # setup basic plot
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # overlay with inweb
  inweb = get_inweb_list('BCL2')
  inweb = list(inweb=inweb[inweb$significant, ])
  p2 = suppressWarnings(plot_overlay(p1, inweb, label = F))

  # no alt label result in no change and NAs are removed
  expect_equal(p2$overlay$dataset, collapse_labels(p2$overlay)$alt_label)
  
  
  # manually changing some alt labels
  overlay = p2$overlay
  index = sample(1:nrow(overlay), size = 3)
  overlay$alt_label[index] <- c('rs1','rs2','rs3')
  res = collapse_labels(overlay)$alt_label[index]
  ref = as.vector(unlist(apply(overlay[index,c('dataset', 'alt_label')], 1, function(x) paste(x, collapse = ': '))))
  expect_equal(ref, res)
  
})








