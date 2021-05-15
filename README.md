
<!-- badges: start -->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/lagelab/Genoppi?branch=master&svg=true)](https://ci.appveyor.com/project/lagelab/genoppi-j8jha)
[![Build Status](https://travis-ci.com/lagelab/Genoppi.svg?branch=master)](https://travis-ci.com/lagelab/Genoppi)
[![Codecov test coverage](https://codecov.io/gh/lagelab/Genoppi/branch/master/graph/badge.svg)](https://codecov.io/gh/lagelab/Genoppi?branch=master)
[![DOI](https://zenodo.org/badge/83465982.svg)](https://zenodo.org/badge/latestdoi/83465982)
<!-- badges: end -->


# Overview

Genoppi is an open-source software for performing quality control and analyzing quantitative proteomic data. In particular, it streamlines the integration of proteomic data with external datasets such as known protein-protein interactions in published literature, data from genetic studies, gene set annotations, or other user-defined inputs.

This README provides instructions for locally installing the Genoppi software in R (>= 3.6), which consists of two main components: an R package and an interactive shiny application. The application is also available at [www.lagelab.org/genoppi](https://www.lagelab.org/genoppi).

In addition, we provide a [welcome guide](inst/shiny-examples/myapp/www/welcome_guide_v1.0.0_210210.pdf) to describe the user interface of the application. The guide also describes the accepted format of various input files in more detail; example input files can be found in *tests/testthat/data*.

Genoppi citation:
Greta Pintacuda*, Frederik H. Lassen*, Yu-Han H. Hsu*, April Kim*, Jacqueline M. Martín, Edyta Malolepsza, Justin K. Lim, Nadine Fornelos, Kevin C. Eggan, Kasper Lage. [Genoppi is an open-source software for robust and standardized integration of proteomic and genetic data](https://doi.org/10.1038/s41467-021-22648-5). Nature Communications 12, 2580 (2021).


## Installation
```R

# download and install Genoppi using the devtools package:
install.packages('devtools')
library(devtools)
devtools::install_github('lagelab/Genoppi')

```

## Launching shiny application
```R

library(genoppi)
launch_genoppi()

```

## Using R package functions

### Example 1: analyzing and visualizing significant changes in protein abundance
```R
  
# processed IP-MS/MS data BCL2 vs IgG control in GPiNs
data('example_data')

# identify significant proteins in bait (BCL2) IP samples compared 
# to controls using a false discovery rate of 0.1
df_stat <- calc_mod_ttest(example_data)
df_sig <- id_enriched_proteins(df_stat, fdr_cutoff = 0.1, logfc_dir = 'positive')

# visualize protein log2 fold change and corresponding statistical significance
volcano <- plot_volcano_basic(df_sig)
volcano <- plot_overlay(volcano, as.bait('BCL2'))
volcano_tidy <- theme_volcano(volcano)
print(volcano_tidy)

scatter <- plot_scatter_basic(df_sig, repA = 'rep1', repB = 'rep2')
scatter_tidy <- theme_scatter(scatter)
print(scatter_tidy)

# customize with classic ggplot notation
volcano_gg <- volcano + ggtitle('BCL2 vs IgG control in GPiNs (Triplicate)')

# use plotly to interact with plots
plt <- make_interactive(volcano_tidy)
add_plotly_layout_volcano(plt)

```

### Example 2: integrating and visualizing auxiliary datasets 
```R

## known InWeb interactors of bait (BCL2)

# note, this must be named list, e.g: 
inweb_list <- list(inweb_df = get_inweb_list('BCL2'))

df_sig %>% 
  plot_volcano_basic() %>%
  plot_overlay(as.bait('BCL2')) %>%
  plot_overlay(inweb_list, label = F) %>%
  theme_volcano() %>%
  make_interactive()

# assess overlap b/w enriched proteins and InWeb interactors
overlap <- calc_hyper(df_sig, inweb_list$inweb_df, 
  data.frame(listName = 'InWeb', intersectN = T), bait = 'BCL2')

# explore results
overlap$statistics

# Venn diagram of overlap
venn <- list(Enriched = overlap$genes$InWeb$success_genes,
  InWeb = overlap$genes$InWeb$sample_genes)
venn_diagram <- draw_genoppi_venn(venn)
plot_venn(venn_diagram, 0.9)


## gene set annotations from HGNC 
genesets <- get_geneset_overlay(df_sig, 'hgnc')

# plot with ggplot
plot_volcano_basic(df_sig) %>%
  plot_overlay(genesets)

# explore via plotly
plot_volcano_basic(df_sig) %>%
  plot_overlay(genesets) %>%
  make_interactive()

```

### Example 3: calculating and visualizing tissue-specific enrichment
```R

# calculate tissue-specific enrichment using GTEx RNA data
gtex_enrichment = lapply_calc_hyper(df_sig, gtex_rna, bait = 'BCL2')
head(gtex_enrichment)

# plot result
plot_tissue_enrichment(gtex_enrichment, 'list_name', col.value = 'BH.FDR', ylab = 'FDR')

# explore 'Brain_Hippocampus' with relatively low FDR
tissue <- get_tissue_list('Brain_Hippocampus',table = gtex_rna)

# view in volcano plot
plot_volcano_basic(df_sig) %>%
  plot_overlay(list(hippocampus=tissue), label = T) %>%
  make_interactive()

```

### Example 4: using significant proteins identified by alternative software as input for downstream Genoppi analyses
```R

# read in example output file generate by SAINTexpress (PMID: 24513533)
df <- read.table('tests/testthat/data/test.SAINTexpress.txt',
  header = T,sep = '\t',stringsAsFactors = F)
df$gene <- df$PreyGene

# define significant proteins (BFDR <= 0.1 in SAINTexpress output)
df$significant <- df$BFDR<=0.1

# perform downstream Genoppi analyses similar to Examples 2-3 above
# e.g. assess overlap with known InWeb interactors
inweb_list <- list(inweb_df = get_inweb_list('BCL2'))

# assess overlap b/w enriched proteins and InWeb interactors
overlap <- calc_hyper(df[,c('gene','significant')], inweb_list$inweb_df,
  data.frame(listName = 'InWeb', intersectN=T), bait = 'BCL2')

# explore results
overlap$statistics

# Venn diagram of overlap
venn <- list(Enriched = overlap$genes$InWeb$success_genes,
  InWeb = overlap$genes$InWeb$sample_genes)
venn_diagram <- draw_genoppi_venn(venn)
plot_venn(venn_diagram, 0.9)

``` 

