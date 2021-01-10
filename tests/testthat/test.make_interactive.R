context('make_interactive')

data("example_data2")

# perform moderated t-test
stats_df <- suppressWarnings(calc_mod_ttest(example_data2))

# identify enriched proteins
sig_df <- id_significant_proteins(stats_df)

test_that('basic test to see if function works',{
  
  # generate volcano plot with bait protein labeled
  basic_volcano <- plot_volcano_basic(sig_df)
  bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2'))
  
  # interactive volcano plot
  result = make_interactive(bait_volcano) %>%
    add_plotly_layout_volcano(width = NULL, height = NULL)
  expect_true(!is.null(result))
  
  # look at annotations
  m = result$x$layoutAttrs[[1]]$annotations
  expect_equal(quo_name(m$x),'logFC')
  expect_equal(quo_name(m$y),'-log10(pvalue)')
  expect_equal(quo_name(m$text),'BCL2')
  
})

test_that('plot without any overlay can be made interactive',{
  
  # generate volcano plot with bait protein labeled
  basic_volcano <- plot_volcano_basic(sig_df)
  result = make_interactive(basic_volcano)
  
  # look at nnotations
  m = result$x$attrs[[2]]
  expect_equal(as.vector(m$colors), c("#41AB5D", "grey"))
  expect_equal(quo_name(m$x),'logFC')
  expect_equal(quo_name(m$y),'-log10(pvalue)')
  expect_equal(quo_name(m$text),'~gene') # no bait
  
})

test_that('plot_overlay arguments are translated to plotly',{

  inweb = list(inweb = get_inweb_list('BCL2'))
  
  # only bait label
  volcano = sig_df %>% 
    plot_volcano_basic() %>%
    plot_overlay(as.bait('BCL2')) %>%
    plot_overlay(inweb, label = F) %>%
    volcano_theme() %>%
    make_interactive()
  
  expect_equal(as.character(volcano$x$layoutAttrs[[1]]$annotations$text), 'BCL2')
  
  # no labels at all
  volcano = sig_df %>% 
    plot_volcano_basic() %>%
    plot_overlay(as.bait('BCL2'), label = F) %>%
    plot_overlay(inweb, label = F) %>%
    volcano_theme() %>%
    make_interactive()
  
  expect_true(identical(as.character(volcano$x$layoutAttrs[[1]]$annotations$text), character(0)))
  
  # only inweb label
  volcano = sig_df %>% 
    plot_volcano_basic() %>%
    plot_overlay(as.bait('BCL2'), label = F) %>%
    plot_overlay(inweb) %>%
    volcano_theme() %>%
    make_interactive()
  
  expect_equal(length(volcano$x$layoutAttrs[[1]]$annotations$text), 12)  
  
})

