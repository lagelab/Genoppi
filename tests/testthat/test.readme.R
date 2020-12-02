context('README')

test_that('Readme data can be run',{
  
  
  ### Visualizing changes in protein abundance
  
  # processed IP-MS/MS data BCL2 vs IgG control in GPiNs
  data("example_data2")
  
  # calculate what proteins are enriched in bait (BCL2) compared 
  # to control using a false discovery rate of 0.1
  df_stat <- calc_mod_ttest(example_data2)
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
  inweb_list = list(inweb_df = get_inweb_list('BCL2'))
  
  df_sig %>% 
    plot_volcano_basic() %>%
    plot_overlay(as.bait('BCL2')) %>%
    plot_overlay(inweb_list, label = F) %>%
    volcano_theme() %>%
    make_interactive()
  
  # assess overlap b/w enriched proteins and InWeb interactors
  overlap = calc_hyper(df_sig, inweb_list$inweb_df, data.frame(listName = 'InWeb', intersectN=T), bait="BCL2")
  overlap$statistics
  
  # Venn diagram of overlap
  venn <- list(Enriched=overlap$genes$InWeb$success_genes, InWeb=overlap$genes$InWeb$sample_genes)
  venn_diagram <- draw_genoppi_venn(venn)
  plot_venn(venn_diagram, 0.9)
  
  ## Integrated analyses using gene set annotations   
  genesets = get_geneset_overlay(df_sig, 'hgnc')
  
  # plot with ggplot
  plot_volcano_basic(df_sig) %>%
    plot_overlay(genesets)
  
  # explore via plotly
  plot_volcano_basic(df_sig) %>%
    plot_overlay(genesets) %>%
    make_interactive()

  
  ### calculating and visualizing tissue-specific enrichment
  
  # look for tissue-specific enrichment
  gtex_enrichment = calc_adjusted_enrichment(df_sig, gtex_rna, bait = 'BCL2')
  head(gtex_enrichment)
  
  # plot result
  plot_tissue_enrichment(gtex_enrichment, 'list_name', col.value = 'BH.FDR', ylab = 'FDR')
  
  # explore Brain_Hippocampus with relatively low FDR
  tissue = get_tissue_lists('Brain_Hippocampus',table=gtex_rna)
  
  # view in volcano plot
  plot_volcano_basic(df_sig) %>%
    plot_overlay(list(hippocampus=tissue), label = T) %>%
    make_interactive()
  
  
})


if (F){
  library(genoppi)
  
  
  data("example_data2")
  
  
  ### ------------------------------------------------------------------
  ### (1) Basic analyses
  
  # perform moderated t-test
  stats_df <- calc_mod_ttest(example_data2)
  
  # identify enriched proteins
  sig_df <- id_enriched_proteins(stats_df)
  
  # generate volcano plot with bait protein labeled
  basic_volcano <- plot_volcano_basic(sig_df)
  bait_volcano <- plot_overlay(basic_volcano,as.bait("BCL2"))
  print(bait_volcano)
  
  # generate correlation scatter plot for two replicates
  basic_scatter <- plot_scatter_basic(sig_df,"rep1","rep2")
  bait_scatter <- plot_overlay(basic_scatter,as.bait("BCL2"))
  print(bait_scatter)
  
  # NOTE: the piping (%>%) command can be used to streamline steps, e.g.: 
  example_data2 %>%
    calc_mod_ttest() %>%
    id_enriched_proteins() %>%
    plot_volcano_basic() %>%
    plot_overlay(as.bait("BCL2")) %>% 
    theme_volcano()
  
  
  # interactive volcano plot
  bait_volcano %>%
    make_interactive() %>%
    add_plotly_layout_volcano()
  
  
  ### ------------------------------------------------------------------
  ### (2) Integrated analyses (using InWeb data as example)
  
  # query InWeb interactors for a bait protein (e.g. BCL2)
  inweb_list <- list(inweb = get_inweb_list("BCL2"))
  plot_overlay(bait_volcano,inweb_list)
  
  # assess overlap b/w enriched proteins and InWeb interactors
  overlap_results <- calc_hyper(sig_df, inweb_df,
                                data.frame(listName="InWeb",intersectN=T), bait="BCL2")
  
  # Venn diagram of overlap
  venn_list <- list(Enriched=overlap_results$genes$InWeb$success_genes,
                    InWeb=overlap_results$genes$InWeb$sample_genes)
  venn_diagram <- draw_genoppi_venn(venn_list)
  
  grid::grid.newpage()
  grid::grid.draw(venn_diagram)
  
  
  ### (3) Integrated analyses using gene set annotations   
  genesets = get_geneset_overlay(sig_df, 'hgnc')
  
  # plot with ggplot
  plot_volcano_basic(sig_df) %>%
    plot_overlay(genesets)
  
  # explore via plotly
  plot_volcano_basic(sig_df) %>%
    plot_overlay(genesets) %>%
    make_interactive()


}
  
  


