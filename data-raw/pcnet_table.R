library(RCX) # tested with v1.2.2 (for reading Cytoscape CX format data)

# read in PCNet data (Huang et al. Cell Systems 2018)
# downloaded from NDEx (UUID: f93f402c-86d4-11e7-a10d-0ac135e8bacf)

# The PCNet_Huang2018.cx file can be directly downloaded from NDEx
# https://www.ndexbio.org/v2/network/f93f402c-86d4-11e7-a10d-0ac135e8bacf?download=true
rcx <- readCX("PCNet_Huang2018.cx")
rcx$metaData # 19781 nodes, 2724724 edges
head(rcx$nodes) # id name represents
head(rcx$edges) # id source target
pcnet_table <- data.frame(
    Gene1 = rcx$nodes$name[match(rcx$edges$source, rcx$nodes$id)],
    Gene2 = rcx$nodes$name[match(rcx$edges$target, rcx$nodes$id)]
)

# save as gzipped tab-delimited file in data-raw/
fwrite(pcnet_table,file='data-raw/pcnet_table.tsv.gz',sep='\t')

# save as .rda file in data/
usethis::use_data(pcnet_table, overwrite=T) # use bzip2 compression by default