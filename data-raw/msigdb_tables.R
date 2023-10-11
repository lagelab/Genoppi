# code to generate msigdb_{collection}_table (colleciton= h, c1-c8)
# laste update: 2023-10-11

# Human MSigDB Collections (v2023.1; need to register/login to download)
# https://www.gsea-msigdb.org/gsea/msigdb/download_file.jsp?filePath=/msigdb/release/2023.1.Hs/msigdb_v2023.1.Hs_files_to_download_locally.zip

# gzip and store downloaded *symbols.gmt files in data-raw/msigdb_v2023.1.Hs_GMTs/
# only use *all.v2023.1.Hs.symbols.gmt.gz file for each collection below (instead of subcollection files)

# H: hallmark gene sets
# C1: positional gene sets
# C2: curated gene sets
# C3: regulatory target gene sets
# C4: computational gene sets
# C5: ontology gene sets
# C6: oncogenic signature gene sets
# C7: immunologic signature gene sets
# C8 : cell type signature gene sets

start_time <- proc.time()

prefixes <- c('h',paste('c',1:8,sep=''))

for (prefix in prefixes) {
	inFile <- paste('data-raw/msigdb_v2023.1.Hs_GMTs/',prefix,'.all.v2023.1.Hs.symbols.gmt.gz',sep='')
	lines <- readLines(inFile)
	
	prefixTable <- NULL
	
	for (line in lines) {
		fields <- unlist(strsplit(line,'\t'))
		prefixTable <- dplyr::bind_rows(prefixTable,
			dplyr::tibble(Gene.symbol=fields[3:length(fields)],Set.name=fields[1]))
	}

	tableName <- paste('msigdb',prefix,'table',sep='_')
	assign(tableName,prefixTable)

	print(tableName)
	print(dim(prefixTable))
	print(length(unique(prefixTable$Gene.symbol)))
	print(length(unique(prefixTable$Set.name)))

	# save as gzipped tab-delimited file in data-raw/
	outFile <- paste('data-raw/',tableName,'.tsv.gz',sep='') 
	data.table::fwrite(get(tableName),file=outFile,sep='\t')

	# save as .rda file (bzip2 compression) in data/
	do.call(usethis::use_data,list(as.name(tableName),overwrite=T))

}

proc.time() - start_time
# finished in ~11 min

