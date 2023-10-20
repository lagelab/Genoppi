#' @title UniProt human protein accession number to gene name mapping
#' @description A dataset containing 167,966 UniProt accession number and gene name pairings. 
#' @format A tibble object with 167,966 rows and 3 columns:
#' \describe{
#'   \item{accession_number}{<chr> UniProt protein accession number (e.g. P10415)}
#'   \item{gene}{<chr> gene name (e.g. BCL2), first gene in all_genes if multiple}
#'   \item{all_genes}{<chr> comma-delimited gene names mapped to accession_number}
#' }
#' @family datasets
#' @docType data
#' @source Human ID mapping file (2023-09-13 release) downloaded from UniProt:  
#' \url{https://www.uniprot.org}
#' @references
#' UniProt Consortium. UniProt: the Universal Protein Knowledgebase in 2023.
#' Nucleic Acids Res. 2023;51(D1):D523-D531. doi:10.1093/nar/gkac1052

"accession_gene_table"


#' @title InWeb protein-protein interactions
#' @description A dataset containing published protein-protein interactions for 18,814 human genes in
#' InWeb_InBioMap (InWeb_IM) or InWeb3.
#' @format A data frame with 883,356 rows and 4 variables:
#' \describe{
#'   \item{Gene1}{HGNC gene symbol of first protein}
#'   \item{Gene2}{HGNC gene symbol of second protein}
#'   \item{Score}{Interaction confidence score}
#'   \item{Source}{InWeb source version, "InWeb_IM" or "InWeb3"}
#' }
#' @family datasets
#' @docType data
#' @source See cited references; InWeb_IM data (2016-9-12 release) downloaded from:
#' \url{https://inbio-discover.intomics.com/map.html#downloads}
#' @references
#' Li T, Wernersson R, Hansen RB, et al. A scored human protein-protein interaction network to catalyze
#' genomic interpretation. Nat Methods. 2017;14(1):61-64. doi:10.1038/nmeth.4083  
#' 
#' Lage K, Karlberg EO, Størling ZM, et al. A human phenome-interactome network of protein complexes implicated
#' in genetic disorders. Nat Biotechnol. 2007;25(3):309-316. doi:10.1038/nbt1295 

"inweb_table"


#' @title BioPlex protein-protein interactions
#' @description A dataset containing 118,162 protein-protein interactions in BioPlex 3.0,
#' identified through AP-MS of 10,128 human bait proteins in HEK293T cells.
#' @format A data.frame with 883,356 rows and 4 variables:
#' \describe{
#'   \item{Gene1}{HGNC gene symbol of first protein (bait)}
#'   \item{Gene2}{HGNC gene symbol of second protein (prey)}
#'   \item{pW}{Probability that the prey is a wrong identification}
#'   \item{pNI}{Probability of the prey is a background protein}
#'   \item{pInt}{Probability of the prey is a high-confidence interacting protein}
#' }
#' @family datasets
#' @docType data
#' @source BioPlex data (v3.0) downloaded on 2021-01-04 from:
#' \url{https://bioplex.hms.harvard.edu/interactions.php}
#' @references
#' Huttlin EL, Ting L, Bruckner RJ, et al. The BioPlex Network: A Systematic Exploration of the Human Interactome.
#' Cell. 2015;162(2):425-440. doi:10.1016/j.cell.2015.06.043
#'
#' Huttlin EL, Bruckner RJ, Navarrete-Perea J, et al. Dual Proteome-scale Networks Reveal Cell-specific Remodeling
#' of the Human Interactome. bioRxiv. 2020. doi:10.1101/2020.01.19.905109 

"bioplex_table"


#' @title iRefIndex protein-protein interactions
#' @description A dataset containing 396,984 published protein-protein interactions for 17,437 human genes
#' in iRefIndex 17.0.
#' @format A data.frame with 396,984 rows and 5 variables:
#' \describe{
#'   \item{Gene1}{HGNC gene symbol of first protein}
#'   \item{Gene2}{HGNC gene symbol of second protein}
#'   \item{Score.hpr.max}{Highest PMID re-use}
#'   \item{Score.lpr.max}{Lowest PMID re-use}
#'   \item{Score.np.max}{Number of PMIDs}
#' }
#' @details This dataset was generated from the source BioPlex database by excluding all non-binary and non-human
#' interactions. HGNC gene symbols were then extracted from the database along with the relevant score columns. 
#' Rows corresponding to the same gene pairs were merged into a single row by keeping the maximum scores. 
#' 
#' @family datasets
#' @docType data
#' @source iRefIndex data (v17.0) downloaded on 2021-01-06 from:
#' \url{https://irefindex.vib.be/download/irefindex/data/archive/release_17.0/psi_mitab/MITAB2.6/}
#' @references
#' Razick S, Magklaras G, Donaldson IM. iRefIndex: a consolidated protein interaction database with provenance.
#' BMC Bioinformatics. 2008;9:405. Published 2008 Sep 30. doi:10.1186/1471-2105-9-405

"irefindex_table"

## COMMENTED OUT FOR NOW: don't add until corresponding .rda file is stored in data/
##' @title HuRI data (HI-union network)
##' @description  TO ADD
##' @format TO ADD
##' \describe{
##'   \item{Gene1}{HGNC gene symbol of first protein}
##'   \item{Gene2}{HGNC gene symbol of second protein}
##' }
##' @details This dataset was generated from the source BioPlex database by excluding all non-binary and non-human
##' interactions. HGNC gene symbols were then extracted from the database along with the relevant score columns.
##' Rows corresponding to the same gene pairs were merged into a single row by keeping the maximum scores.
##'
##' @family datasets
##' @docType data
##' @source HuRI data (HI-union network) data downloaded on 2023-10-10 from:
##' \url{https://www.nature.com/articles/s41586-020-2188-x}
##' TODO: try direct download from https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-020-2188-x/MediaObjects/41586_2020_2188_MOESM3_ESM.zip
##' @references
##' Luck, K., Kim, DK., Lambourne, L. et al. A reference map of the human binary protein interactome. Nature 580, 402–408
##' (2020). https://doi.org/10.1038/s41586-020-2188-x

##"huri_table"


#' @title SNP-to-gene mapping 
#' @description A dataset containing 19,326 protein-coding genes (GENCODE v43) mapped to SNP IDs (dbSNP build 156).
#' A gene is linked to a SNP if its genomic coordinates overlap with linkage disequilibrium (LD) locus
#' boundaries of the SNP. LD loci were calculated using the 1000 Genomes phase 3 EUR panel and PLINK,
#' defined as regions spanning tagging SNPs (r2 > 0.6) ± 50kb; SNPs with MAF < 0.05 were excluded. 
#' @format A hash object with 19,326 gene names as keys and vectors of SNP IDs as values
#' @family datasets
#' @docType data
#' @source GENCODE v43 data downloaded from:
#' \url{https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_43/gencode.v43.basic.annotation.gtf.gz}
#' 1000 Genomes phase 3 data (with dbSNP build 156 annotation) downloaded from:
#' \url{https://www.cog-genomics.org/plink/2.0/resources#phase3_1kg}
#' @references#
#' Frankish A, Diekhans M, Jungreis I, et al. GENCODE 2021.
#' Nucleic Acids Res. 2021;49(D1):D916-D923. doi:10.1093/nar/gkaa1087
#'
#' Byrska-Bishop M, Evani US, Zhao X, et al. High-coverage whole-genome sequencing of the expanded 1000 Genomes
#' Project cohort including 602 trios. Cell. 2022;185(18):3426-3440.e19. doi:10.1016/j.cell.2022.08.004
#'
#' Chang CC, Chow CC, Tellier LC, Vattikuti S, Purcell SM, Lee JJ. Second-generation PLINK: rising to the challenge
#' of larger and richer datasets. Gigascience. 2015;4:7. Published 2015 Feb 25. doi:10.1186/s13742-015-0047-8

"genes_snps_hash"


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


#' @title HGNC gene groups
#' @description A dataset containing annotations for 25,637 genes in 1,622 HGNC gene groups. 
#' @format A tibble object with 30,791 rows and 3 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Group.ID}{<int> Gene group ID}
#'   \item{Group.name}{<chr> Gene group name}
#' }
#' @family datasets
#' @docType data
#' @source HGNC genes within groups dataset downloaded on 2023-10-02 from:
#' \url{https://www.genenames.org/download/statistics-and-files/}
#' @references
#' Seal RL, Braschi B, Gray K, et al. Genenames.org: the HGNC resources in 2023.
#' Nucleic Acids Res. 2023;51(D1):D1003-D1009. doi:10.1093/nar/gkac888

"hgnc_group_table"


#' @title GO molecular function terms
#' @description A dataset containing annotations for 18,172 genes in 4,614 GO molecular function terms.
#' @format A tibble object with 292,611 rows and 3 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{GO.ID}{<chr> GO term ID}
#'   \item{GO.name}{<chr> GO term name}
#' }
#' @family datasets
#' @docType data
#' @source Homo sapiens EBI Gene Ontology Annotation Database protein dataset (2023-07-27 release) downloaded from:
#' \url{http://current.geneontology.org/products/pages/downloads.html}
#' @references
#' Ashburner M, Ball CA, Blake JA, et al. Gene ontology: tool for the unification of biology.
#' The Gene Ontology Consortium. Nat Genet. 2000;25(1):25-29. doi:10.1038/75556  
#'
#' Gene Ontology Consortium, Aleksander SA, Balhoff J, et al. The Gene Ontology knowledgebase in 2023.
#' Genetics. 2023;224(1):iyad031. doi:10.1093/genetics/iyad031

"goa_mf_table"


#' @title GO cellular component terms
#' @description A dataset containing annotations for 18,868 genes in 1,800 GO cellular component terms.
#' @format A tibble object with 179,867 rows and 3 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{GO.ID}{<chr> GO term ID}
#'   \item{GO.name}{<chr> GO term name}
#' }
#' @family datasets
#' @docType data
#' @source Homo sapiens EBI Gene Ontology Annotation Database protein dataset (2023-07-27 release) downloaded from:
#' \url{http://current.geneontology.org/products/pages/downloads.html}
#' @references
#' Ashburner M, Ball CA, Blake JA, et al. Gene ontology: tool for the unification of biology.
#' The Gene Ontology Consortium. Nat Genet. 2000;25(1):25-29. doi:10.1038/75556  
#'
#' Gene Ontology Consortium, Aleksander SA, Balhoff J, et al. The Gene Ontology knowledgebase in 2023.
#' Genetics. 2023;224(1):iyad031. doi:10.1093/genetics/iyad031

"goa_cc_table"


#' @title GO biological process terms
#' @description A dataset containing annotations for 17,775 genes in 12,335 GO biological process terms.
#' @format A tibble object with 161,108 rows and 3 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{GO.ID}{<chr> GO term ID}
#'   \item{GO.name}{<chr> GO term name}
#' }
#' @family datasets
#' @docType data
#' @source Homo sapiens EBI Gene Ontology Annotation Database protein dataset (2023-07-27 release) downloaded from:
#' \url{http://current.geneontology.org/products/pages/downloads.html}
#' @references
#' Ashburner M, Ball CA, Blake JA, et al. Gene ontology: tool for the unification of biology.
#' The Gene Ontology Consortium. Nat Genet. 2000;25(1):25-29. doi:10.1038/75556  
#'
#' Gene Ontology Consortium, Aleksander SA, Balhoff J, et al. The Gene Ontology knowledgebase in 2023.
#' Genetics. 2023;224(1):iyad031. doi:10.1093/genetics/iyad031

"goa_bp_table"


#' @title MSigDB H collection (hallmark gene sets)
#' @description A dataset containing annotations for 4,384 genes in 50 MSigDB hallmark gene sets.
#' @format A tibble object with 7,322 rows and 2 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Set.name}{<chr> Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source MSigDB Human Gene Set GMT file set (v2023.1.Hs release) downloaded from:
#' \url{https://www.gsea-msigdb.org/gsea/downloads.jsp}
#' @references
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
#' The Molecular Signatures Database (MSigDB) hallmark gene set collection.
#' Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 
#'
#' See additional references at: https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp

"msigdb_h_table"


#' @title MSigDB C1 collection (positional gene sets)
#' @description A dataset containing annotations for 42,198 genes in 300 MSigDB positional gene sets.
#' @format A tibble object with 42,242 rows and 2 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Set.name}{<chr> Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source MSigDB Human Gene Set GMT file set (v2023.1.Hs release) downloaded from:
#' \url{https://www.gsea-msigdb.org/gsea/downloads.jsp}
#' @references
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
#' The Molecular Signatures Database (MSigDB) hallmark gene set collection.
#' Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 
#'
#' See additional references at: https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp

"msigdb_c1_table"


#' @title MSigDB C2 collection (curated gene sets)
#' @description A dataset containing annotations for 22,109 genes in 6,495 MSigDB curated gene sets.
#' @format A tibble object with 538,934 rows and 2 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Set.name}{<chr> Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source MSigDB Human Gene Set GMT file set (v2023.1.Hs release) downloaded from:
#' \url{https://www.gsea-msigdb.org/gsea/downloads.jsp}
#' @references
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
#' The Molecular Signatures Database (MSigDB) hallmark gene set collection.
#' Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 
#'
#' See additional references at: https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp

"msigdb_c2_table"


#' @title MSigDB C3 collection (regulatory target gene sets)
#' @description A dataset containing annotations for 28,651 genes in 3,713 MSigDB regulatory target gene sets.
#' @format A tibble object with 818,113 rows and 2 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Set.name}{<chr> Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source MSigDB Human Gene Set GMT file set (v2023.1.Hs release) downloaded from:
#' \url{https://www.gsea-msigdb.org/gsea/downloads.jsp}
#' @references
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
#' The Molecular Signatures Database (MSigDB) hallmark gene set collection.
#' Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 
#'
#' See additional references at: https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp

"msigdb_c3_table"


#' @title MSigDB C4 collection (computational gene sets)
#' @description A dataset containing annotations for 10,013 genes in 858 MSigDB computational gene sets.
#' @format A tibble object with 91,175 rows and 2 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Set.name}{<chr> Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source MSigDB Human Gene Set GMT file set (v2023.1.Hs release) downloaded from:
#' \url{https://www.gsea-msigdb.org/gsea/downloads.jsp}
#' @references
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
#' The Molecular Signatures Database (MSigDB) hallmark gene set collection.
#' Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 
#'
#' See additional references at: https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp

"msigdb_c4_table"


#' @title MSigDB C5 collection (ontology gene sets)
#' @description A dataset containing annotations for 19,384 genes in 15,937 MSigDB ontology gene sets.
#' @format A tibble object with 1,293,170 rows and 2 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Set.name}{<chr> Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source MSigDB Human Gene Set GMT file set (v2023.1.Hs release) downloaded from:
#' \url{https://www.gsea-msigdb.org/gsea/downloads.jsp}
#' @references
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
#' The Molecular Signatures Database (MSigDB) hallmark gene set collection.
#' Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 
#'
#' See additional references at: https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp

"msigdb_c5_table"


#' @title MSigDB C6 collection (oncogenic signature gene sets)
#' @description A dataset containing annotations for 10,931 genes in 189 MSigDB oncogenic signature gene sets.
#' @format A tibble object with 30,579 rows and 2 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Set.name}{<chr> Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source MSigDB Human Gene Set GMT file set (v2023.1.Hs release) downloaded from:
#' \url{https://www.gsea-msigdb.org/gsea/downloads.jsp}
#' @references
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
#' The Molecular Signatures Database (MSigDB) hallmark gene set collection.
#' Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 
#'
#' See additional references at: https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp

"msigdb_c6_table"


#' @title MSigDB C7 collection (immunologic signature gene sets)
#' @description A dataset containing annotations for 21,383 genes in 5,219 MSigDB immunologic signature gene sets.
#' @format A tibble object with 990,381 rows and 2 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Set.name}{<chr> Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source MSigDB Human Gene Set GMT file set (v2023.1.Hs release) downloaded from:
#' \url{https://www.gsea-msigdb.org/gsea/downloads.jsp}
#' @references
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
#' The Molecular Signatures Database (MSigDB) hallmark gene set collection.
#' Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 
#'
#' See additional references at: https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp

"msigdb_c7_table"


#' @title MSigDB C8 collection (cell type signature gene sets)
#' @description A dataset containing annotations for 20,393 genes in 830 MSigDB cell type signature gene sets.
#' @format A tibble object with 149,795 rows and 2 columns:
#' \describe{
#'   \item{Gene.symbol}{<chr> HGNC gene symbol}
#'   \item{Set.name}{<chr> Gene set name}
#' }
#' @family datasets
#' @docType data
#' @source MSigDB Human Gene Set GMT file set (v2023.1.Hs release) downloaded from:
#' \url{https://www.gsea-msigdb.org/gsea/downloads.jsp}
#' @references
#' Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
#' The Molecular Signatures Database (MSigDB) hallmark gene set collection.
#' Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004 
#'
#' See additional references at: https://www.gsea-msigdb.org/gsea/msigdb/human/collection_details.jsp

"msigdb_c8_table"


#' @title Human Protein Atlas (HPA) RNA tissue specificity annotations
#' @description A dataset containing annotations for 19,114 tissue-elevated genes in 36 tissues derived from 
#' HPA consensus transcriptomics data. See references for definition of tissue-elevated genes.
#' @format A tibble object with 688,104 rows and 3 columns:
#' \describe{
#'   \item{tissue}{<chr> Tissue name}
#'   \item{gene}{<chr> HGNC gene symbol}
#'   \item{significant}{<lgl> whether or not gene is tissue-elevated}
#' }
#' @family datasets
#' @docType data
#' @source Human Protein Atlas data (v23.0) downloaded from: \url{https://www.proteinatlas.org/about/download}  
#' @references
#' Uhlén M, Fagerberg L, Hallström BM, et al. Proteomics. Tissue-based map of the human proteome.
#' Science. 2015;347(6220):1260419. doi:10.1126/science.1260419 
#'
#' See additional documentation at: https://www.proteinatlas.org/humanproteome/tissue/tissue+specific

"hpa_rna_table"


#' @title GTEx RNA tissue specificity annotations
#' @description A dataset containing annotations for 17,589 tissue-specific genes in 53 tissues derived from
#' GTEx RNA-seq data. See references for definition of tissue-specific genes. In brief, genes that rank
#' in the top decile of a t-statistic distribution are defined as the tissue-specific genes in each tissue.
#' @format A tibble object with 932,217 rows and 4 columns:
#' \describe{
#'   \item{tissue}{<chr> Tissue name}
#'   \item{category}{<chr> Tissue category}
#'   \item{gene}{<chr> HGNC gene symbol}
#'   \item{significant}{<lgl> whether or not the gene is tissue-specific} 
#' }
#' @family datasets
#' @docType data
#' @source GTEx.tstat.tsv downloaded on 2023-10-13 from:
#' \url{https://console.cloud.google.com/storage/browser/broad-alkesgroup-public-requester-pays/LDSCORE/LDSC_SEG_ldscores/tstats/}
#' @references
#' Finucane HK, Reshef YA, Anttila V, et al. Heritability enrichment of specifically expressed genes identifies
#' disease-relevant tissues and cell types. Nat Genet. 2018;50(4):621-629. doi:10.1038/s41588-018-0081-4

"gtex_rna_table"


#' @title GTEx RNA tissue specificity annotations for brain regions/tissues
#' @description A dataset containing annotations for 17,589 tissue-specific genes in 13 brain tissues derived from
#' GTEx RNA-seq data. See references for definition of tissue-specific genes. In brief, genes that rank
#' in the top decile of a t-statistic distribution are defined as the tissue-specific genes in each tissue.
#' @format A tibble object with 228,657 rows and 3 columns:
#' \describe{
#'   \item{tissue}{<chr> Tissue name}
#'   \item{gene}{<chr> HGNC gene symbol}
#'   \item{significant}{<lgl> whether or not the gene is tissue-specific} 
#' }
#' @family datasets
#' @docType data
#' @source GTEx_brain.tstat.tsv downloaded on 2023-10-13 from:
#' \url{https://console.cloud.google.com/storage/browser/broad-alkesgroup-public-requester-pays/LDSCORE/LDSC_SEG_ldscores/tstats/}
#' @references
#' Finucane HK, Reshef YA, Anttila V, et al. Heritability enrichment of specifically expressed genes identifies
#' disease-relevant tissues and cell types. Nat Genet. 2018;50(4):621-629. doi:10.1038/s41588-018-0081-4

"gtex_brain_table"


#' @title GTEx protein tissue specificity annotations
#' @description A dataset containing annotations for 12,325 tissue-enriched genes in 32 tissues derived from
#' GTEx proteomics data. See references for definition of tissue-enriched genes.
#' @format A tibble object with 394,400 rows and 3 columns:
#' \describe{
#'   \item{tissue}{<chr> Tissue name}
#'   \item{gene}{<chr> HGNC gene symbol}
#'   \item{significant}{<lgl> whether or not the gene is tissue-enriched} 
#' }
#' @family datasets
#' @docType data
#' @source Table S4 of Jiang et al. downloaded on 2023-10-13 from:
#' \url{https://www.sciencedirect.com/science/article/pii/S0092867420310783}
#' @references
#' Jiang L, Wang M, Lin S, et al. A Quantitative Proteome Map of the Human Body.
#' Cell. 2020;183(1):269-283.e19. doi:10.1016/j.cell.2020.08.036

"gtex_protein_table"


#' @title Example proteomic data (1)
#' @description A dataset containing gene symbol and processed protein abundance values derived from 
#' BCL2 (bait) vs. IgG (control) immunoprecipitation-mass spectrometry experiment in a neuron cell line (GPiN).
#' @format A data frame with 446 rows and 10 variables:
#' \describe{
#'   \item{gene}{HGNC gene symbol}
#'   \item{sample1}{log2 protein intensity in 1st replicate of bait IP}
#'   \item{control1}{log2 protein intensity in 1st replicate of control IP}
#'   \item{sample2}{log2 protein intensity in 2nd replicate of bait IP}
#'   \item{control2}{log2 protein intensity in 2nd replicate of control IP}
#'   \item{sample3}{log2 protein intensity in 3rd replicate of bait IP}
#'   \item{control3}{log2 protein intensity in 3rd replicate of control IP}
#'   \item{rep1}{log2 fold change (bait over control) of replicate 1}
#'   \item{rep2}{log2 fold change (bait over control) of replicate 2}
#'   \item{rep3}{log2 fold change (bait over control) of replicate 3}
#' }
#' @family datasets
#' @docType data
#' @source Genoppi IP-MS experiment
#' @references
#' Pintacuda G, Lassen FH, Hsu YH, Kim A, Martín JM, Malolepsza E et al. Genoppi is an open-source software
#' for robust and standardized integration of proteomic and genetic data. Nat Commun. 2021;12(1):2580.
#' doi:10.1038/s41467-021-22648-5

"example_data"


#' @title Example proteomic data (2)
#' @description A dataset containing gene symbol and processed protein abundance values derived from 
#' BCL2 (bait) vs. IgG (control) immunoprecipitation-mass spectrometry experiment in a cancer cell line (A375).
#' @format A data frame with 510 rows and 10 variables:
#' \describe{
#'   \item{gene}{HGNC gene symbol}
#'   \item{sample1}{log2 protein intensity in 1st replicate of bait IP}
#'   \item{control1}{log2 protein intensity in 1st replicate of control IP}
#'   \item{sample2}{log2 protein intensity in 2nd replicate of bait IP}
#'   \item{control2}{log2 protein intensity in 2nd replicate of control IP}
#'   \item{sample3}{log2 protein intensity in 3rd replicate of bait IP}
#'   \item{control3}{log2 protein intensity in 3rd replicate of control IP}
#'   \item{rep1}{log2 fold change (bait over control) of replicate 1}
#'   \item{rep2}{log2 fold change (bait over control) of replicate 2}
#'   \item{rep3}{log2 fold change (bait over control) of replicate 3}
#' }
#' @family datasets
#' @docType data
#' @source Genoppi IP-MS experiment
#' @references
#' Pintacuda G, Lassen FH, Hsu YH, Kim A, Martín JM, Malolepsza E et al. Genoppi is an open-source software
#' for robust and standardized integration of proteomic and genetic data. Nat Commun. 2021;12(1):2580.
#' doi:10.1038/s41467-021-22648-5

"example_data2"


#' @title Example proteomic data (3)
#' @description A dataset containing gene symbol and processed protein abundance values derived from 
#' BCL2 (bait) vs. IgG (control) immunoprecipitation-mass spectrometry experiment in a cancer cell line (G401).
#' @format A data frame with 508 rows and 10 variables:
#' \describe{
#'   \item{gene}{HGNC gene symbol}
#'   \item{sample1}{log2 protein intensity in 1st replicate of bait IP}
#'   \item{control1}{log2 protein intensity in 1st replicate of control IP}
#'   \item{sample2}{log2 protein intensity in 2nd replicate of bait IP}
#'   \item{control2}{log2 protein intensity in 2nd replicate of control IP}
#'   \item{sample3}{log2 protein intensity in 3rd replicate of bait IP}
#'   \item{control3}{log2 protein intensity in 3rd replicate of control IP}
#'   \item{rep1}{log2 fold change (bait over control) of replicate 1}
#'   \item{rep2}{log2 fold change (bait over control) of replicate 2}
#'   \item{rep3}{log2 fold change (bait over control) of replicate 3}
#' }
#' @family datasets
#' @docType data
#' @source Genoppi IP-MS experiment
#' @references
#' Pintacuda G, Lassen FH, Hsu YH, Kim A, Martín JM, Malolepsza E et al. Genoppi is an open-source software
#' for robust and standardized integration of proteomic and genetic data. Nat Commun. 2021;12(1):2580.
#' doi:10.1038/s41467-021-22648-5

"example_data3"

