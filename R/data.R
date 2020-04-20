#' @title  gene ID mapping table
#' @description A table containing 67,744 entries of accession_number to gene (HGNC symbol) mapping. 
#' @format data.frame object with columns: accession_number, gene
#' @family datasets
#' @docType data
#' @source Protein-coding genes downloaded from Uniprot.

"accession_gene_table"


#' @title InWeb hash table
#' @description A hash table containing InWeb interactions for 18,478 genes (generated October 2018).
#' @format hash object with each of 18,478 genes as key, vector of its InWeb interactors as value
#' @family datasets
#' @docType data
#' @source Genoppi-v3 InWeb_Combined_Oct2018.Rdata file created by April Kim.

"inweb_hash"


#' @title SNP-to-gene mapping hash table
#' @description A hash table containing 18,116 genes mapped to LD loci of 1KG phase 3 SNPs.
#' @format hash object with gene name as key, vector of SNPs as value
#' @family datasets
#' @docType data
#' @source Genoppi-v3 snp_to_gene.Rdata file created by April Kim.

"genes_snps"


#' @title  GWAS catalog table
#' @description A table containing 190,668 genetic associations in GWAS catalog. SNP data are based on dbSNP Build 152.
#' @format data.frame object with: PUBMEDID, DISEASE.TRAIT, SNP, P.VALUE, SUTDY.ACCESSION
#' @family datasets
#' @docType data
#' @source gwas_catalog_v1.0.2-associations_e98_r2020-03-08.tsv file downloaded from GWAS catalog: https://www.ebi.ac.uk/gwas/docs/file-downloads.

"gwas_table"


#' @title gnomAD table
#' @description The gnomAD v2 data set contains data from 125,748 exomes and 15,708 whole genomes, all mapped to the GRCh37/hg19 reference sequence.
#' @format data.frame from gnomAD V 2.1.1
#' @family datasets
#' @docType data
#' @source https://storage.googleapis.com/gnomad-public/release/2.1.1/constraint/gnomad.v2.1.1.lof_metrics.by_gene.txt.bgz

"gnomad_table"


#' @title HGNC gene group annotation table
#' @description A table containing 28,677 gene group annotations from HGNC (23,874 unique symbols, 1415 unique groups).
#' @format data.frame object with: Gene.symbol, Group.name
#' @family datasets
#' @docType data
#' @source HGNC genes within groups dataset. Downloaded on 2020-03-30 (https://www.genenames.org/download/statistics-and-files/).

"hgnc_group_table"


#' @title GOA molecular function annotation table
#' @description A table containing 65,891 GO molecular function annotations (17,654 unique gene symbols, 4,271 unique GO MF terms).
#' @format data.frame object with: Gene.symbol, GO.ID, GO.name
#' @family datasets
#' @docType data
#' @source GOA Homo sapiens protein dataset, 2020-03-23 release. Downloaded on 2020-03-30 (http://current.geneontology.org/products/pages/downloads.html).

"goa_mf_table"


#' @title GOA cellular component annotation table
#' @description A table containing 83,029 GO cellular component annotations (18,880 unique gene symbols, 1,765 unique GO CC terms).
#' @format data.frame object with: Gene.symbol, GO.ID, GO.name
#' @family datasets
#' @docType data
#' @source GOA Homo sapiens protein dataset, 2020-03-23 release. Downloaded on 2020-03-30 (http://current.geneontology.org/products/pages/downloads.html).

"goa_cc_table"


#' @title GOA biological process annotation table
#' @description A table containing 140,067 GO biological process annotations (17,814 unique gene symbols, 12,362 unique GO BP terms).
#' @format data.frame object with: Gene.symbol, GO.ID, GO.name
#' @family datasets
#' @docType data
#' @source GOA Homo sapiens protein dataset, 2020-03-23 release. Downloaded on 2020-03-30 (http://current.geneontology.org/products/pages/downloads.html).

"goa_bp_table"


#' @title MSigDB H collection (hallmark gene sets) annotation table
#' @description A table containing 7,321 MSigDB hallmark gene set annotations (4,383 unique gene symbols, 50 unique gene sets).
#' @format data.frame object with: Gene.symbol, Set.name
#' @family datasets
#' @docType data
#' @source h.all.v7.1.symbols.gmt file downloaded on 2020-04-03 (https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#H).

"msigdb_h_table"


#' @title MSigDB C1 collection (positional gene sets) annotation table
#' @description A table containing 38,340 MSigDB positional gene set annotations (38,340 unique gene symbols, 299 unique gene sets).
#' @format data.frame object with: Gene.symbol, Set.name
#' @family datasets
#' @docType data
#' @source c1.all.v7.1.symbols.gmt file downloaded on 2020-04-03 (https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#H).

"msigdb_c1_table"


#' @title MSigDB C2 collection (curated gene sets) annotation table
#' @description A table containing 479,444 MSigDB curated gene set annotations (20,738 unique gene symbols, 5,529 unique gene sets).
#' @format data.frame object with: Gene.symbol, Set.name
#' @family datasets
#' @docType data
#' @source c2.all.v7.1.symbols.gmt file downloaded on 2020-04-03 (https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#H).

"msigdb_c2_table"


#' @title MSigDB C3 collection (regulatory target gene sets) annotation table
#' @description A table containing 843,716 MSigDB regulatory target gene set annotations (26,647 unique gene symbols, 3,735 unique gene sets).
#' @format data.frame object with: Gene.symbol, Set.name
#' @family datasets
#' @docType data
#' @source c3.all.v7.1.symbols.gmt file downloaded on 2020-04-03 (https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#H).

"msigdb_c3_table"


#' @title MSigDB C4 collection (computational gene sets) annotation table
#' @description A table containing 91,037 MSigDB computational gene set annotations (10,004 unique gene symbols, 858 unique gene sets).
#' @format data.frame object with: Gene.symbol, Set.name
#' @family datasets
#' @docType data
#' @source c4.all.v7.1.symbols.gmt file downloaded on 2020-04-03 (https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#H).

"msigdb_c4_table"


#' @title MSigDB C5 collection (GO gene sets) annotation table
#' @description A table containing 868,350 MSigDB GO gene set annotations (18,046 unique gene symbols, 10,192 unique gene sets).
#' @format data.frame object with: Gene.symbol, Set.name
#' @family datasets
#' @docType data
#' @source c5.all.v7.1.symbols.gmt file downloaded on 2020-04-03 (https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#H).

"msigdb_c5_table"


#' @title MSigDB C6 collection (oncogenic signatures) annotation table
#' @description A table containing 30,469 MSigDB oncogenic signature annotations (10,897 unique gene symbols, 189 unique gene sets).
#' @format data.frame object with: Gene.symbol, Set.name
#' @family datasets
#' @docType data
#' @source c6.all.v7.1.symbols.gmt file downloaded on 2020-04-03 (https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#H).

"msigdb_c6_table"


#' @title MSigDB C7 collection (immunologic signatures) annotation table
#' @description A table containing 947,586 MSigDB immunologic signature annotations (20,235 unique gene symbols, 4872 unique gene sets).
#' @format data.frame object with: Gene.symbol, Set.name
#' @family datasets
#' @docType data
#' @source c7.all.v7.1.symbols.gmt file downloaded on 2020-04-03 (https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#H).

"msigdb_c7_table"


#' @title example proteomic data
#' @description Simulated proteomic dataset containing gene symbol and corresponding log2 fold change values for 3 replciates
#' @format data.table object with columns: gene, rep1, rep2, rep3
#' @family datasets
#' @docType data
#' @source simulated data, do not represent true biology. 

"example_data"

