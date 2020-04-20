context('get_snp_lists')

# SNP list test file
snpFile <- "data/test.snps.txt" # two columns with header: listName, SNP

# SNP data.frame
snpDf <- read.table("data/test.snps.txt",header=T,sep="\t")

# genes
genes <- as.factor(c("ATXN2","PFN1","FUS","MATR3","SOD1","PRPH"))
genes_null <- as.factor(c("TEST1","TEST2","TEST3"))

test_that('get_snp_lists can return correct data.frame',{

  # SNP list file, with mapped genes overlapping proteomic data
  result <- get_snp_lists(snpFile, genes)
  expect_identical(sort(as.character(unique(result$listName))),c("List1","List2"))
  expect_identical(sort(as.character(result$gene)),c("ATXN2","FUS","MATR3","SOD1","SOD1"))

  # SNP data.frame (same data as above)
  result2 <- get_snp_lists(snpDf, genes)
  expect_identical(result,result2)

  # SNP list with no overlap in proteomic data
  result2 <- get_snp_lists(snpFile, genes_null)
  expect_true(is.null(result2))

})


test_that('single column snp produces an error',{
  
  # only one column gives an error
  expect_error(get_snp_lists(as.vector(snpDf[,2]), genes))
  
  # incorrectly named collumsn give an errpr
  snpDf_copy <- snpDf
  colnames(snpDf_copy) <- c('bla', 'SNP')
  expect_error(get_snp_lists(snpDf_copy, genes))
  
})

