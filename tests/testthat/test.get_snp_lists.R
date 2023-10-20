context('get_snp_lists')

test_that('get_snp_lists can return correct data.frame',{
  
  # test data
  snpFile <- 'data/test.snps.txt' # two columns with header: listName, SNP
  snpDf <- read.table('data/test.snps.txt',header=T,sep='\t')
  genes <- c('ATXN2','PFN1','FUS','MATR3','SOD1','PRPH')

  expected <- data.frame(listName=c('List1','List1','List1','List2'),
    gene=c('ATXN2','FUS','MATR3','SOD1'),
    SNP=c('rs848132','rs749767','rs877826','rs10483006'))

  # SNP list file as input
  result <- get_snp_lists(snpFile, genes)
  expect_identical(result,expected)	

  # SNP data.frame as input
  result2 <- get_snp_lists(snpDf, genes)
  expect_identical(result2,expected)

  # SNP list with no overlap in input genes
  result3 <- get_snp_lists(snpFile, c('BADNAME1','BADNAME2'))
  expect_true(is.null(result3))

})


test_that('single column snp produces an error',{
  
  # only one column gives an error
  expect_error(get_snp_lists(as.vector(snpDf[,2]), genes))
  
  # incorrectly named collumns give an errpr
  snpDf_copy <- snpDf
  colnames(snpDf_copy) <- c('bla', 'SNP')
  expect_error(get_snp_lists(snpDf_copy, genes))
  
})

