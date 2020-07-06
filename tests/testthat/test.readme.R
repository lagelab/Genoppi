context('README')



test_that('Readme data can be run',{
  
  
  ### Visualizing changes in protein abundance
  
  # processed IP-MS/MS data BCL2 vs IgG control in GPiNs
  data("example_data")
  
  # calculate what proteins are enriched in bait (BCL2) compared 
  # to control using a false discovery rate of 0.1
  df_stat <- calc_mod_ttest(example_data)
  df_sig <- id_enriched_proteins(df_stat, fdr_cutoff = 0.1, logfc_dir = 'positive')
    
  # visualize enrichment
  volcano <- plot_volcano_basic(df_sig)
  volcano <- plot_overlay(volcano, as.bait('BCL2'))
  volcano_tidy <- volcano_theme(volcano)
  print(volcano_tidy)
  
  scatter <- plot_scatter_basic(df_sig, repA='rep1', repB='rep2')
  scatter_tidy <- scatter_theme(scatter, -10, 10)
  print(scatter_tidy)
  
  # customize with classic ggplot notation
  volcano_gg <- volcano +
    ggtitle('BCL2 vs IgG control in GPiNs (Triplicate)')
  
  # use plotly to interact with plots
  plt <- make_interactive(volcano_tidy)
  add_plotly_layout_volcano(plt)

  
  ### Integrating and visulizing auxilliary genetic data 

  ## enrichment of InWeb
  
  # note, this must be named list, e.g: 
  inweb = list(inweb = get_inweb_list('BCL2'))
  
  df_sig %>% 
    plot_volcano_basic() %>%
    plot_overlay(as.bait('BCL2')) %>%
    plot_overlay(inweb, label = F) %>%
    volcano_theme(ylims = c(0, 4)) %>%
    make_interactive()
  
  # Calculate p-value for enrichment
  
  # assess overlap b/w enriched proteins and InWeb interactors

  
  
})






