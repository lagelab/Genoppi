
<!-- badges: start -->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/lagelab/Genoppi?branch=master&svg=true)](https://ci.appveyor.com/project/lagelab/genoppi-j8jha)
[![Build Status](https://travis-ci.com/lagelab/Genoppi.svg?branch=master)](https://travis-ci.com/lagelab/Genoppi)
[![Codecov test coverage](https://codecov.io/gh/lagelab/Genoppi/branch/master/graph/badge.svg)](https://codecov.io/gh/lagelab/Genoppi?branch=master)
<!-- badges: end -->



# Overview

Genoppi is an open-source software for performing quality control and analyzing quantitative proteomic data. In particular, it streamlines the integration of proteomic data with external datasets such as known protein-protein interactions in published literature, data from genetic studies, gene set annotations, or other user-defined inputs.

This README provides instructions for locally installing the Genoppi software in R (>= 3.6), which consists of two main components: an R package and an interactive shiny application. 

In addition, we provide a [welcome guide](inst/shiny-examples/myapp/www/welcome_guide_200415.pdf) to describe the user interface of the application. The guide also describes the accepted format of various input files.


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

## R package usage example

```R

library(genoppi)

# load example proteomic data
data("example_data")


### ------------------------------------------------------------------
### (1) Basic analyses

# perform moderated t-test
stats_df <- calc_mod_ttest(example_data)

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
example_data %>%
  calc_mod_ttest() %>%
  id_enriched_proteins() %>%
  plot_volcano_basic() %>%
  plot_overlay(as.bait("BCL2"))


# interactive volcano plot
make_interactive(bait_volcano) %>%
  add_plotly_layout_volcano()


### ------------------------------------------------------------------
### (2) Integrated analyses (using InWeb data as example)

# query InWeb interactors for a bait protein (e.g. BCL2)
inweb_df <- data.frame(listName="InWeb",get_inweb_list("BCL2"))

# overlaid volcano plot labeling InWeb interactors
inweb_list <- list(InWeb=inweb_df[inweb_df$significant, ])
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


```

