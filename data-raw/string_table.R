library(data.table) # also requires R.utils for fread() to read in gz/bz2 files
# read in STRING data (physical subnetwork, Szklarczyk et al. Nucleic Acids Res 2021)
# downloaded from: https://string-db.org/cgi/download?sessionId=bpij0JN28bsF
# restricting to Homo sapiens, physical interactions

# TODO check if link download works the same as read table
gene_url <- 'https://stringdb-downloads.org/download/protein.info.v12.0/9606.protein.info.v12.0.txt.gz'
string_genes <- data.table::fread(gene_url,header=T, sep = "\t", stringsAsFactors = F) # should automatically skip comment lines starting with '!'

int_url <- 'https://stringdb-downloads.org/download/protein.physical.links.v12.0/9606.protein.physical.links.v12.0.txt.gz'
string_ints <- data.table::fread(int_url,header=T, sep = " ", stringsAsFactors = F)

names(string_genes)[1:3] <- c("protein_id", "gene_name", "protein_size")
# ~400 genes with ENSG IDs in gene name column, keep as is for now

string_table <- data.frame(
    Gene1 = string_genes$gene_name[match(string_ints$protein1, string_genes$protein_id)],
    Gene2 = string_genes$gene_name[match(string_ints$protein2, string_genes$protein_id)]
)

# save as gzipped tab-delimited file in data-raw/ç
fwrite(string_table,file='data-raw/string_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(string_table, overwrite=T) # use bzip2 compression by default