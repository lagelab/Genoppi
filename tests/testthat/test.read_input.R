context('read_input')

#setwd('tests/testthat/')

test_that('read_input can return input data.frame and format',{
 
  # make artificial data
  df1 <- data.frame(gene=letters, rep1=rnorm(26), rep2=rnorm(26), rep3=rnorm(26))
  tmp <- tempfile()
  write.table(df1, tmp, quote=F, sep="\t", row.names=F)
  
  # check returned data.frame 
  result <- read_input(tmp, sep="\t")
  expect_equal(result$data, df1)
 
  # check returned data format list
  expected_check <- list(gene_rep=T, accession_rep=F, gene_signif=F, accession_signif=F)
  expect_identical(result$format$check, expected_check)
 
})


test_that('an input with all columns can be read',{
  
  # data with all columns can be read
  df <- read.table(file = 'data/BCL2vsIgG.txt', header = T)
  tmp <- tempfile()
  write.table(df, tmp, quote=F, sep="\t", row.names=F)
  
  # check returned data.frame 
  result <- read_input(tmp, sep="\t")
  expect_equal(result$data, df)
  
  # check returned data format list
  expected_check <- list(gene_rep=T, accession_rep=T, gene_signif=T, accession_signif=T)
  expect_identical(result$format$check, expected_check)
  
})


test_that('only accession numbers and reps',{
  
  # data with all columns can be read
  df <- read.table(file = 'data/BCL2vsIgG.txt', header = T)
  df$gene <- NULL
  df$logFC <- NULL
  df$pvalue <- NULL
  df$FDR <- NULL
  tmp <- tempfile()
  write.table(df, tmp, quote=F, sep="\t", row.names=F)
  
  # check returned data.frame 
  result <- read_input(tmp, sep="\t")
  expect_equal(result$data, df)
  
  # check returned data format list
  expected_check <- list(gene_rep=F, accession_rep=T, gene_signif=F, accession_signif=F)
  expect_identical(result$format$check, expected_check)
  
})


test_that('only accession numbers and statistics',{
  
  # data with all columns can be read
  df <- read.table(file = 'data/BCL2vsIgG.txt', header = T)
  df$gene <- NULL
  df$rep1 <- NULL
  df$rep2 <- NULL
  df$rep3 <- NULL
  tmp <- tempfile()
  write.table(df, tmp, quote=F, sep="\t", row.names=F)
  
  # check returned data.frame 
  result <- read_input(tmp, sep="\t")
  expect_equal(result$data, df)
  
  # check returned data format list
  expected_check <- list(gene_rep=F, accession_rep=F, gene_signif=F, accession_signif=T)
  expect_identical(result$format$check, expected_check)
  
})









