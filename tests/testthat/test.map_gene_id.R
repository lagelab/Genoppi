context('map_gene_id')



test_that('map_gene_id correctly maps acession_number (uniprot) to HGNC' ,{
  
  # read in test data
  df <- data.frame(accession_number=c('Q96DR7', 'Q13148', 'P17948'), rep1=1:3, rep2=4:6)
  outDf <- data.frame(df,gene=c('ARHGEF26','TARDBP', 'FLT1'))
  
  df2 <- data.frame(accession_number=c('Q96DR7','Q96DR7-3'))
  outDf2 <- data.frame(accession_number=c('Q96DR7','Q96DR7-3'),gene=c('ARHGEF26','ARHGEF26'))
  
  df3 <- data.frame(accession_number=c('BADNAME'))
  
  # typical cased
  result <- map_gene_id(df)
  result$gene <- as.factor(result$gene)
  outDf$gene <- as.factor(outDf$gene)
  expect_equal(result, outDf)
  
  # valid Uniprot ID but with isoform
  result <- map_gene_id(df2)
  result$gene <- as.factor(result$gene)
  outDf2$gene <- as.factor(outDf2$gene)
  expect_equal(result, outDf2)
  
  # invalid uniprot ID
  result <- map_gene_id(df3)
  expect_true(is.na(result$gene))
  
})


test_that('Full pipeline with accession number and reps',{
  
  # data with all columns can be read
  df <- read.table(file = 'data/BCL2vsIgG.txt', header = T)
  df$gene <- NULL
  df$logFC <- NULL
  df$pvalue <- NULL
  df$FDR <- NULL
  tmp <- tempfile()
  write.table(df, tmp, quote=F, sep="\t", row.names=F)
  
  # check returned data.frame 
  d_in <- read_input(tmp, sep="\t")
  expect_equal(d_in$data, df)
  mapped <- head(map_gene_id(d_in$data))
  expect_equal(as.character(mapped$gene), c('APPL1','MOV10','SRGAP1','ATXN2','APC2','MCRIP1'))
  
})

test_that('Full pipeline with accession numbers and signif',{
  
  # data with all columns can be read
  df <- read.table(file = 'data/BCL2vsIgG.txt', header = T)
  df$gene <- NULL
  df$rep1 <- NULL
  df$rep2 <- NULL
  df$rep3 <- NULL
  tmp <- tempfile()
  write.table(df, tmp, quote=F, sep="\t", row.names=F)
  
  # check returned data.frame 
  d_in <- read_input(tmp, sep="\t")
  expect_equal(d_in$data, df)
  mapped <- head(map_gene_id(d_in$data))
  expect_equal(as.character(mapped$gene), c('APPL1','MOV10','SRGAP1','ATXN2','APC2','MCRIP1'))
  
})

test_that('Some accession numbers are not found mapped',{
  
  df <- data.frame(accession_number=c('BLABLA', 'Q13148', 'BLABLA3'), rep1=1:3, rep2=4:6)
  outDf <- data.frame(df,gene=c(NA,'TARDBP', NA), stringsAsFactors = T)
  result <- map_gene_id(df)
  expect_equal(result$gene, outDf$gene)
  
})




