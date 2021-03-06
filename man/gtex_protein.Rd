% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{gtex_protein}
\alias{gtex_protein}
\title{GTEx protein tissue specificity annotations}
\format{
A data frame with 686,624 rows and 3 variables:
\describe{
  \item{tissue}{Tissue name}
  \item{gene}{HGNC gene symbol}
  \item{significant}{Logical variable indicating whether or not the gene is tissue-specific} 
}
}
\source{
Table S2 (TS dataset) downloaded on 2020-10-21 from:
\url{https://www.sciencedirect.com/science/article/pii/S0092867420310783}
}
\usage{
gtex_protein
}
\description{
A dataset containing tissue specificity annotations for 12,184 genes in 32 tissues derived from
proteomic data. See cited reference for details on the definition of tissue-specific genes.
}
\references{
Jiang L, Wang M, Lin S, et al. A Quantitative Proteome Map of the Human Body.
Cell. 2020;183(1):269-283.e19. doi:10.1016/j.cell.2020.08.036
}
\seealso{
Other datasets: 
\code{\link{accession_gene_table}},
\code{\link{bioplex_table}},
\code{\link{example_data2}},
\code{\link{example_data3}},
\code{\link{example_data}},
\code{\link{genes_snps}},
\code{\link{gnomad_table}},
\code{\link{goa_bp_table}},
\code{\link{goa_cc_table}},
\code{\link{goa_mf_table}},
\code{\link{gtex_rna}},
\code{\link{gwas_table}},
\code{\link{hgnc_group_table}},
\code{\link{hpa_rna}},
\code{\link{inweb_table}},
\code{\link{irefindex_table}},
\code{\link{msigdb_c1_table}},
\code{\link{msigdb_c2_table}},
\code{\link{msigdb_c3_table}},
\code{\link{msigdb_c4_table}},
\code{\link{msigdb_c5_table}},
\code{\link{msigdb_c6_table}},
\code{\link{msigdb_c7_table}},
\code{\link{msigdb_h_table}}
}
\concept{datasets}
\keyword{datasets}
