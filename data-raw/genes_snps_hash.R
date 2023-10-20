# code to generate genes_snps_hash
# last update: 2023-10-20

start_time <- Sys.time()

library(dplyr)

# ----------------------------------------------------------------------------------------
# Documentation for SNPs LD data ('data-raw/snps_ld/chr*.tags.list.subCols.gz' files)
# (1) download 1KG phase 3 GRCh38 genotype data from PLINK2 Resources page
#     (https://www.cog-genomics.org/plink/2.0/resources#phase3_1kg)
#     2022-08-04 Byrska-Bishop et al. callset files:
#          all_hg38.pgen.zst
#          all_hg38_rs_noannot.pvar.zst	(with dbSNP 156 rsIDs)
#          hg38_corrected.psam		(with KING-based pedigree corrections)
#          deg2_hg38.king.cutoff.out.id	(1st and 2nd degree related samples)	
# (2) decompress pgen file
#     plink2 --zst-decompress all_hg38.pgen.zst > all_hg38.pgen
# (3) subset psam file for EUR population
#     grep -e 'EUR' -e '#' hg38_corrected.psam > hg38_corrected_eur.psam
# (2) convert to PLINK1 binary, filter for EUR unrelated samples and chr1-22,X SNPs
#     plink2 --pfile all_hg38 --make-bed \
#     --pgen all_hg38.pgen --pvar all_hg38_rs_noannot.pvar.zst --psam hg38_corrected.psam \ 
#     --keep hg38_corrected_eur.psam --remove deg2_hg38.king.cutoff.out.id \
#     --allow-extra-chr --chr 1-22,X \
#     --out 1kgp3_hg38_eur_nodeg2_chr1-22X
# (3) identify LD boundaries for each SNP with MAF > 0.05 and missing rate < 0.1,
#     based on tagging SNPs with r2 > 0.6 in 200kb window (use PLINK 1.9)
#     plink_linux_x86_64_20230116/plink \
#     --bfile 1kgp3_hg38_eur_nodeg2_chr1-22X \
#     --chr ${chr} --maf 0.05 --geno 0.1 \
#     --show-tags all --tag-r2 0.6 --tag-kb 200 \
#     --out chr${chr}
# (4) save relevant columns (SNP CHR LEFT RIGHT) in gzipped file
#     awk '{OFS="\t"; print $1,$2,$5,$6}' chr${chr}.tags.list | \
#     gzip > chr${chr}.tags.list.subCols.gz

# ----------------------------------------------------------------------------------------
# download GENCODE v43 GTF for GRCh38 gene coordinate annotations
url <- 'https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_43/gencode.v43.basic.annotation.gtf.gz'
genDt <- data.table::fread(url,header=F,sep='\t')

# extract relevant rows and columns for protein-coding genes
# with gene names on chr1-22,X
genDt <- dplyr::filter(genDt,V3=='gene') %>%
	tidyr::separate_wider_delim(V9,delim='; ',names=c('gene_id','gene_type','gene_name'),too_many='drop') %>%
	dplyr::mutate(gene_type=gsub('gene_type |\"','',gene_type),
		gene_name=gsub('gene_name |\"','',gene_name)) %>%
	dplyr::filter(gene_type=='protein_coding' & 
		!grepl('ENSG',gene_name) & !V1 %in% c('chrY','chrM')) %>% 
	dplyr::select(gene=gene_name,chr=V1,start=V4,end=V5)

dim(genDt) # 19360 x 4
length(unique(genDt$gene)) # 19349 unique gene names (some genes have >1 chr pos)
sum(genDt$end < genDt$start) # confirm all end pos > start pos (for interval overlap check below to work)

# check gene names with multiple entries
#x <- names(which(table(genDt$gene)>1)) # 10 gene names
#y <- data.frame(subset(genDt,gene %in% x))
#y[order(y$gene),] # all entries of same gene name show up on same chr

# collapse these entries by keeping min start and max end
genDt <- dplyr::group_by(genDt,gene) %>% summarize(chr=unique(chr),start=min(start),end=max(end))
dim(genDt) # 19349 x 4


# hash object: gene name as key, vector of SNPs as value
genes_snps_hash <- hash::hash()

# iterate through each chr
for (chrom in c(1:22,'X')) {
	print(paste('CHR',chrom))

	# SNPs LD data for the chromosome
	snpDt <- data.table::fread(paste0('data-raw/snps_ld/chr',chrom,'.tags.list.subCols.gz'),header=T)

	# extend LD boundaries by +-50kb
	snpDt$LEFT <- snpDt$LEFT - 50000
	snpDt$RIGHT <- snpDt$RIGHT + 50000

	# iterate through each gene on chromosome
	for (g in subset(genDt,chr==paste0('chr',chrom))$gene) {
		# boolean, whether SNP LD intervals overlap with gene coords
		overlap <- (subset(genDt,gene==g)$start <= snpDt$RIGHT) &
			(subset(genDt,gene==g)$end >= snpDt$LEFT)
		
		# store all SNPs whose LD intervals overlap with gene in hash object
		if (sum(overlap) > 0) {	
			hash::.set(genes_snps_hash,keys=g,values=snpDt$SNP[overlap])
		}
	}
}

length(hash::keys(genes_snps_hash)) # 19326 genes linked to SNPs

# save as .rda file in data/
usethis::use_data(genes_snps_hash, overwrite=T)

Sys.time() - start_time

