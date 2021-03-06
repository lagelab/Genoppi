% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{gwas_table}
\alias{gwas_table}
\title{NHGRI-EBI GWAS catalog genetic associations}
\format{
A data frame with 188,925 rows and 5 variables:
\describe{
  \item{PUBMEDID}{PubMed identification number}
  \item{DISEASE.TRAIT}{Disease or trait examined in study}
  \item{SNP}{SNP rsID}
  \item{P.VALUE}{Reported p-value}
  \item{STUDY.ACCESSION}{Accession ID allocated to a GWAS catalog study}
}
}
\source{
All associations v1.0.2 dataset (2020-03-08 release) downloaded on 2020-03-17 from:
\url{https://www.ebi.ac.uk/gwas/docs/file-downloads}
}
\usage{
gwas_table
}
\description{
A dataset containing 188,925 published trait-SNP associations in the NHGRI-EBI GWAS catalog.
}
\references{
Buniello A, MacArthur JAL, Cerezo M, et al. The NHGRI-EBI GWAS Catalog of published genome-wide association studies,
targeted arrays and summary statistics 2019. Nucleic Acids Res. 2019;47(D1):D1005-D1012. doi:10.1093/nar/gky1120
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
\code{\link{gtex_protein}},
\code{\link{gtex_rna}},
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
