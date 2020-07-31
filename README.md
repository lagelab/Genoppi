
<!-- badges: start -->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/lagelab/Genoppi?branch=master&svg=true)](https://ci.appveyor.com/project/lagelab/genoppi-j8jha)
[![Build Status](https://travis-ci.com/lagelab/Genoppi.svg?branch=master)](https://travis-ci.com/lagelab/Genoppi)
[![Codecov test coverage](https://codecov.io/gh/lagelab/Genoppi/branch/master/graph/badge.svg)](https://codecov.io/gh/lagelab/Genoppi?branch=master)
<!-- badges: end -->



# Overview

Genoppi is an open-source software for performing quality control and analyzing quantitative proteomic data. In particular, it streamlines the integration of proteomic data with external datasets such as known protein-protein interactions in published literature, data from genetic studies, gene set annotations, or other user-defined inputs.

This README provides instructions for locally installing the Genoppi software in R (>= 3.6), which consists of two main components: an R package and an interactive shiny application. The application is also available at [www.lagelab.org/genoppi](https://www.lagelab.org/genoppi).

In addition, we provide a [welcome guide](inst/shiny-examples/myapp/www/welcome_guide_200509.pdf) to describe the user interface of the application. The guide also describes the accepted format of various input files in more detail; example input files can be found in *tests/testthat/data*.


## Installation

```R

# download and install Genoppi using the devtools package:
install.packages("devtools")
library(devtools)
devtools::install_github("lagelab/Genoppi")

```

## Launching shiny application

```R

library(genoppi)
launch_genoppi()

```


## Getting started

### example 1: Visualizing changes in protein abundance
```R
  
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

```


### example 2: Integrating and visualizing auxilliary genetic data 
```R

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
  overlap = calc_hyper(df_sig, 
    inweb_list$inweb_df, 
    data.frame(listName = 'InWeb', intersectN=T), bait="BCL2")
  
  # explore result
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



```

### calculating and visualizing tissue-specific enrichment

```R
  # look for tissue-specific enrichment
  gtex_enrichment = calc_adjusted_enrichment(df_sig, gtex_table, bait = 'BCL2')
  head(gtex_enrichment)
  
  # plot result
  plot_tissue_enrichment(gtex_enrichment, 'list_name', col.value = 'BH.FDR', ylab = 'FDR')
  
  # explore Brain_Hippocampus with relatively low FDR
  tissue = get_tissue_lists('Brain_Hippocampus',table=gtex_table)
  
  # view in volcano plot
  plot_volcano_basic(df_sig) %>%
    plot_overlay(list(hippocampus=tissue), label = T) %>%
    make_interactive()
```


