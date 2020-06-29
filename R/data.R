#' @title Uniprot protein accession number to HGNC gene symbol mapping
#' @description A dataset containing 67,519 Uniprot accession number to HGNC gene symbol pairings. 
#' @format A data frame with 67,519 rows and 2 variables:
#' \describe{
#'   \item{accession_number}{Uniprot accession number (e.g. P10415)}
#'   \item{gene}{HGNC gene symbol (e.g. BCL2)}
#' }
#' @family datasets
#' @docType data
#' @source ID mapping data for human protein-coding genes downloaded from Uniprot:  
#' \url{https://www.uniprot.org}
#' @references
#' UniProt Consortium. UniProt: a worldwide hub of protein knowledge. Nucleic Acids Res.
#' 2019;47(D1):D506-D515. doi:10.1093/nar/gky1049

"accession_gene_table"


#' @title InWeb_InBioMap protein-protein interactions
#' @description A dataset containing published protein-protein interactions for 18,551 genes in InWeb_InBioMap.
#' @format A hash object with each of 18,551 genes as key and a vector of its InWeb interactors as value
#' @family datasets
#' @docType data
#' @source InWeb_Combined_Oct2018.Rdata file created by April Kim, modified to add 73 genes as keys.
#' @references
#' Li T, Wernersson R, Hansen RB, et al. A scored human protein-protein interaction network to
#' catalyze genomic interpretation. Nat Methods. 2017;14(1):61-64. doi:10.1038/nmeth.4083

"inweb_hash"


#' @title 1000 Genomes SNP to HGNC gene symbol mapping
#' @description A dataset containing 1000 Genomes phase 3 single-nucleotide polymorphsims (SNPs) mapped to 
#' 18,116 gene symbols, using linkage disequilibrium information from the CEU population. See cited Genoppi
#' reference for details on mapping method. 
#' @format A hash object with each of 18,116 genes as key and a vector of SNPs as value
#' @family datasets
#' @docType data
#' @source snp_to_gene.Rdata file created by April Kim.
#' @references
#' 1000 Genomes Project Consortium, Auton A, Brooks LD, et al. A global reference for human genetic variation.
#' Nature. 2015;526(7571):68-74. doi:10.1038/nature15393 
#'
#' Pintacuda G, Lassen FH, Hsu Y-HH, Kim A, Martín JM, Malolepsza E et al. Genoppi: an open-source software
#' for robust and standardized integration of proteomic and genetic data. bioRxiv. 2020. doi:10.1101/2020.05.04.076034  

"genes_snps"


#' @title NHGRI-EBI GWAS catalog genetic associations
#' @description A dataset containing 188,925 published trait-SNP associations in the NHGRI-EBI GWAS catalog.
#' @format A data frame with 188,925 rows and 5 variables:
#' \describe{
#'   \item{PUBMEDID}{PubMed identification number}
#'   \item{DISEASE.TRAIT}{Disease or trait examined in study}
#'   \item{SNP}{SNP rsID}
#'   \item{P.VALUE}{Reported p-value}
#'   \item{STUDY.ACCESSION}{Accession ID allocated to a GWAS catalog study}
#' }
#' @family datasets
#' @docType data
#' @source All associations v1.0.2 dataset (2020-03-08 release) downloaded on 2020-03-17 from:
#' \url{https://www.ebi.ac.uk/gwas/docs/file-downloads}
#' @references
#' Buniello A, MacArthur JAL, Cerezo M, et al. The NHGRI-EBI GWAS Catalog of published genome-wide association studies,
#' targeted arrays and summary statistics 2019. Nucleic Acids Res. 2019;47(D1):D1005-D1012. doi:10.1093/nar/gky1120 

"gwas_table"


#' @title gnomAD mutational constraint (pLI) annotations
#' @description A dataset containing gnomAD pLI scores for 19,704 genes. The pLI score estimates the probability
#' that a gene is intolerant of loss-of-function mutations.
#' @format A data frame with 19,704 rows and 2 variables:
#' \describe{
#'   \item{gene}{HGNC gene symbol}
#'   \item{pLI}{pLI score}
#' } 
#' @family datasets
#' @docType data
#' @source pLoF Metrics by Gene dataset (v2.1.1) downloaded from:
#' \url{https://gnomad.broadinstitute.org/downloads#v2-constraint}
#' @references
#' Lek M, Karczewski KJ, Minikel EV, et al. Analysis of protein-coding genetic variation in 60,706 humans.
#' Nature. 2016;536(7616):285-291. doi:10.1038/nature19057
#'
#' Karczewski KJ, Francioli LC, Tiao G, et al. The mutational constraint spectrum quantified from variation in 141,456 humans.
#' Nature. 2020;581(7809):434-443. doi:10.1038/s41586-020-2308-7 

"gnomad_table"


#' @title HGNC gene group annotations
#' @description A dataset containing annotations for 23,874 genes in 1,415 HGNC gene groups. 
#' @format A data frame with 28,677 rows and 2 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{Group.name}{Gene group name}
#' }
#' @family datasets
#' @docType data
#' @source HGNC genes within groups dataset downloaded on 2020-03-30 from:
#' \url{https://www.genenames.org/download/statistics-and-files/}
#' @references
#' Yates B, Braschi B, Gray KA, Seal RL, Tweedie S, Bruford EA. Genenames.org: the HGNC and VGNC resources in 2017.
#' Nucleic Acids Res. 2017;45(D1):D619-D625. doi:10.1093/nar/gkw1033 

"hgnc_group_table"


#' @title GO molecular function annotations
#' @description A dataset containing annotations for 17,654 genes in 4,271 GO molecular funtion terms.
#' @format A data frame with 65,891 rows and 3 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{GO.ID}{GO term ID}
#'   \item{GO.name}{GO term name}
#' }
#' @family datasets
#' @docType data
#' @source Homo sapiens EBI Gene Ontology Annotation Database protein dataset (2020-03-23 release) downloaded on 2020-03-30 from:
#' \url{http://current.geneontology.org/products/pages/downloads.html}
#' @references
#' Ashburner M, Ball CA, Blake JA, et al. Gene ontology: tool for the unification of biology.
#' The Gene Ontology Consortium. Nat Genet. 2000;25(1):25-29. doi:10.1038/75556
#'
#' The Gene Ontology Consortium. The Gene Ontology Resource: 20 years and still GOing strong.
#' Nucleic Acids Res. 2019;47(D1):D330-D338. doi:10.1093/nar/gky1055

"goa_mf_table"


#' @title GO cellular component annotations
#' @description A dataset containing annotations for 18,880 genes in 1,765 GO cellular component terms.
#' @format A data frame with 83,029 rows and 3 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{GO.ID}{GO term ID}
#'   \item{GO.name}{GO term name}
#' }
#' @family datasets
#' @docType data
#' @source Homo sapiens EBI Gene Ontology Annotation Database protein dataset (2020-03-23 release) downloaded on 2020-03-30 from:
#' \url{http://current.geneontology.org/products/pages/downloads.html}
#' @references
#' Ashburner M, Ball CA, Blake JA, et al. Gene ontology: tool for the unification of biology.
#' The Gene Ontology Consortium. Nat Genet. 2000;25(1):25-29. doi:10.1038/75556
#'
#' The Gene Ontology Consortium. The Gene Ontology Resource: 20 years and still GOing strong.
#' Nucleic Acids Res. 2019;47(D1):D330-D338. doi:10.1093/nar/gky1055

"goa_cc_table"


#' @title GO biological process annotations
#' @description A dataset containing annotations for 17,814 genes in 12,362 GO biological process terms.
#' @format A data frame with 140,067 rows and 3 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{GO.ID}{GO term ID}
#'   \item{GO.name}{GO term name}
#' }
#' @family datasets
#' @docType data
#' @source Homo sapiens EBI Gene Ontology Annotation Database protein dataset (2020-03-23 release) downloaded on 2020-03-30 from:
#' \url{http://current.geneontology.org/products/pages/downloads.html}
#' @references
#' Ashburner M, Ball CA, Blake JA, et al. Gene ontology: tool for the unification of biology.
#' The Gene Ontology Consortium. Nat Genet. 2000;25(1):25-29. doi:10.1038/75556
#'
#' The Gene Ontology Consortium. The Gene Ontology Resource: 20 years and still GOing strong.
#' Nucleic Acids Res. 2019;47(D1):D330-D338. doi:10.1093/nar/gky1055

"goa_bp_table"


#' @title MSigDB H collection (hallmark gene sets) annotations
#' @description A dataset containing annotations for 4,383 genes in 50 MSigDB hallmark gene sets.
#' @format A data frame with 7,321 rows and 2 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{Set.name}{Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source H: hallmark gene sets dataset (v7.1) downloaded on 2020-04-03 from:
#' \url{https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp}
#' @references
#' Subramanian A, Tamayo P, Mootha VK, et al. Gene set enrichment analysis: a knowledge-based approach for interpreting
#' genome-wide expression profiles. Proc Natl Acad Sci U S A. 2005;102(43):15545-15550. doi:10.1073/pnas.0506580102 
#'
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P. The Molecular Signatures Database (MSigDB)
#' hallmark gene set collection. Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 

"msigdb_h_table"


#' @title MSigDB C1 collection (positional gene sets) annotations
#' @description A dataset containing annotations for 38,340 genes in 299 MSigDB positional gene sets.
#' @format A data frame with 38,340 rows and 2 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{Set.name}{Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source C1: positional gene sets dataset (v7.1) downloaded on 2020-04-03 from:
#' \url{https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp}
#' @references
#' Subramanian A, Tamayo P, Mootha VK, et al. Gene set enrichment analysis: a knowledge-based approach for interpreting
#' genome-wide expression profiles. Proc Natl Acad Sci U S A. 2005;102(43):15545-15550. doi:10.1073/pnas.0506580102 
#'
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P. The Molecular Signatures Database (MSigDB)
#' hallmark gene set collection. Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004

"msigdb_c1_table"


#' @title MSigDB C2 collection (curated gene sets) annotations
#' @description A dataset containing annotations for 20,738 genes in 5,529 MSigDB curated gene sets.
#' @format A data frame with 479,444 rows and 2 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{Set.name}{Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source C2: curated gene sets dataset (v7.1) downloaded on 2020-04-03 from:
#' \url{https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp}
#' @references
#' Subramanian A, Tamayo P, Mootha VK, et al. Gene set enrichment analysis: a knowledge-based approach for interpreting
#' genome-wide expression profiles. Proc Natl Acad Sci U S A. 2005;102(43):15545-15550. doi:10.1073/pnas.0506580102 
#'
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P. The Molecular Signatures Database (MSigDB)
#' hallmark gene set collection. Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 

"msigdb_c2_table"


#' @title MSigDB C3 collection (regulatory target gene sets) annotations
#' @description A dataset containing annotations for 26,647 genes in 3,735 MSigDB regulatory target gene sets.
#' @format A data frame with 843,716 rows and 2 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{Set.name}{Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source C3: regulatory target gene sets dataset (v7.1) downloaded on 2020-04-03 from:
#' \url{https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp}
#' @references
#' Subramanian A, Tamayo P, Mootha VK, et al. Gene set enrichment analysis: a knowledge-based approach for interpreting
#' genome-wide expression profiles. Proc Natl Acad Sci U S A. 2005;102(43):15545-15550. doi:10.1073/pnas.0506580102 
#'
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P. The Molecular Signatures Database (MSigDB)
#' hallmark gene set collection. Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 

"msigdb_c3_table"


#' @title MSigDB C4 collection (computational gene sets) annotations
#' @description A dataset containing annotations for 10,004 genes in 858 MSigDB computational gene sets.
#' @format A data frame with 91,037 rows and 2 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{Set.name}{Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source C4: computational gene sets dataset (v7.1) downloaded on 2020-04-03 from:
#' \url{https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp}
#' @references
#' Subramanian A, Tamayo P, Mootha VK, et al. Gene set enrichment analysis: a knowledge-based approach for interpreting
#' genome-wide expression profiles. Proc Natl Acad Sci U S A. 2005;102(43):15545-15550. doi:10.1073/pnas.0506580102 
#'
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P. The Molecular Signatures Database (MSigDB)
#' hallmark gene set collection. Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 

"msigdb_c4_table"


#' @title MSigDB C5 collection (GO gene sets) annotations
#' @description A dataset containing annotations for 18,046 genes in 10,192 MSigDB GO gene sets.
#' @format A data frame with 868,350 rows and 2 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{Set.name}{Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source C5: GO gene sets dataset (v7.1) downloaded on 2020-04-03 from:
#' \url{https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp}
#' @references
#' Subramanian A, Tamayo P, Mootha VK, et al. Gene set enrichment analysis: a knowledge-based approach for interpreting
#' genome-wide expression profiles. Proc Natl Acad Sci U S A. 2005;102(43):15545-15550. doi:10.1073/pnas.0506580102 
#'
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P. The Molecular Signatures Database (MSigDB)
#' hallmark gene set collection. Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 

"msigdb_c5_table"


#' @title MSigDB C6 collection (oncogenic signatures) annotations
#' @description A dataset containing annotations for 10,897 genes in 189 MSigDB oncogenic signatures.
#' @format A data frame with 30,469 rows and 2 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{Set.name}{Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source C6: oncogenic signatures dataset (v7.1) downloaded on 2020-04-03 from:
#' \url{https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp}
#' @references
#' Subramanian A, Tamayo P, Mootha VK, et al. Gene set enrichment analysis: a knowledge-based approach for interpreting
#' genome-wide expression profiles. Proc Natl Acad Sci U S A. 2005;102(43):15545-15550. doi:10.1073/pnas.0506580102 
#'
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P. The Molecular Signatures Database (MSigDB)
#' hallmark gene set collection. Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 

"msigdb_c6_table"


#' @title MSigDB C7 collection (immunologic sig natures) annotations
#' @description A dataset containing annotations for 20,235 genes in 4,872 MSigDB immunologic signatures.
#' @format A data frame with 947,586 rows and 2 variables:
#' \describe{
#'   \item{Gene.symbol}{HGNC gene symbol}
#'   \item{Set.name}{Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source C7: immunologic signatures dataset (v7.1) downloaded on 2020-04-03 from:
#' \url{https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp}
#' @references
#' Subramanian A, Tamayo P, Mootha VK, et al. Gene set enrichment analysis: a knowledge-based approach for interpreting
#' genome-wide expression profiles. Proc Natl Acad Sci U S A. 2005;102(43):15545-15550. doi:10.1073/pnas.0506580102 
#'
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P. The Molecular Signatures Database (MSigDB)
#' hallmark gene set collection. Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 

"msigdb_c7_table"


#' @title Human Protein Atlas (HPA) tissue specificity annotations
#' @description A dataset containing tissue specificity annotations for 19,634 genes in 33 tissues derived from 
#' HPA RNA-sequencing data. See cited reference and \url{https://www.proteinatlas.org/humanproteome/tissue/tissue+specific}
#' for details on definition of tissue-specific (i.e. tissue-elevated) genes. 
#' @format A data frame with 571,504 rows and 3 variables:
#' \describe{
#'   \item{tissue}{Tissue name}
#'   \item{gene}{HGNC gene symbol}
#'   \item{significant}{Logical variable: "TRUE" = gene has elevated expression in tissue;
#'                      "FALSE" = gene is detected (but not elevated) in tissue} 
#' }
#' @family datasets
#' @docType data
#' @source normal_tissue.tsv.zip and proteinatlas.tsv.zip datasets (v19.3) downloaded on 2020-06-12 from:
#' \url{https://www.proteinatlas.org/about/download}  
#' @references
#' Uhlén M, Fagerberg L, Hallström BM, et al. Proteomics. Tissue-based map of the human proteome.
#' Science. 2015;347(6220):1260419. doi:10.1126/science.1260419 

"hpa_table"


#' @title GTEx tissue specificity annotations
#' @description A dataset containing tissue specificty annotations for 20,025 genes in 53 tissues derived from
#' GTEx RNA-sequencing data. See cited reference for details on the definition of tissue-specific genes.
#' In brief, genes that rank in the top 10 percent of a tissue-based t-statistic distribution are defined as
#' the specific genes for the tissue.
#' @format A data frame with 1,316,785 rows and 3 variables:
#' \describe{
#'   \item{tissue}{Tissue name}
#'   \item{gene}{HGNC gene symbol}
#'   \item{significant}{Logical variable indicating whether or not the gene is tissue-specific} 
#' }
#' @family datasets
#' @docType data
#' @source GTEx.tstat.tsv dataset (2018-06-25 release) downloaded on 2020-06-15 from:
#' \url{https://data.broadinstitute.org/alkesgroup/LDSCORE/LDSC_SEG_ldscores/tstats/}):.
#' @references
#' Finucane HK, Reshef YA, Anttila V, et al. Heritability enrichment of specifically expressed genes identifies
#' disease-relevant tissues and cell types. Nat Genet. 2018;50(4):621-629. doi:10.1038/s41588-018-0081-4

"gtex_table"


#' @title Example proteomic data (1)
#' @description Simulated proteomic dataset containing gene symbol and corresponding log2 fold change values for 3 replicates.
#' @format A data frame with 556 rows and 4 variables:
#' \describe{
#'   \item{gene}{HGNC gene symbol}
#'   \item{rep1}{replicate 1 log2 fold change}
#'   \item{rep2}{replicate 2 log2 fold change}
#'   \item{rep3}{replicate 3 log2 fold change}
#' }
#' @family datasets
#' @docType data

"example_data"


#' @title Example proteomic data (2)
#' @description Simulated proteomic dataset containing gene symbol and corresponding log2 fold change values for 3 replicates.
#' @format A data frame with 685 rows and 4 variables:
#' \describe{
#'   \item{gene}{HGNC gene symbol}
#'   \item{rep1}{replicate 1 log2 fold change}
#'   \item{rep2}{replicate 2 log2 fold change}
#'   \item{rep3}{replicate 3 log2 fold change}
#' }
#' @family datasets
#' @docType data

"example_data2"


#' @title Example proteomic data (3)
#' @description Simulated proteomic dataset containing gene symbol and corresponding log2 fold change values for 3 replicates.
#' @format A data frame with 577 rows and 4 variables:
#' \describe{
#'   \item{gene}{HGNC gene symbol}
#'   \item{rep1}{replicate 1 log2 fold change}
#'   \item{rep2}{replicate 2 log2 fold change}
#'   \item{rep3}{replicate 3 log2 fold change}
#' }
#' @family datasets
#' @docType data

"example_data3"

