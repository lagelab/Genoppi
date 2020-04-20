context('get_gene_lists')

# ALS genes test files
geneFile <- "data/test.ALSgenes.txt" # file with a single gene column
sigFile <- "data/test.ALSgenes.sigCol.txt" # file with significant column
nonSigFile <- "data/test.ALSgenes.nonSig.txt" # file containing significant=F, multiple listNames

test_that('get_gene_lists can return correct data.frame',{

  # input file with gene column
  result <- get_gene_lists(geneFile,sep="\t")
  expect_equal(length(result[[1]]$gene),54)
  expect_equal(sum(result[[1]]$significant),54)
  expect_false(result[[2]]$intersectN)

  # input file with gene and significant column
  result2 <- get_gene_lists(sigFile)
  expect_identical(result,result2)

  # input file with non-significant genes
  result3 <- get_gene_lists(nonSigFile,"\t")
  expect_true(all(result3[[1]]$listName %in% c("List1","List2")))
  expect_equal(length(result3[[1]]$gene),56)
  expect_equal(sum(result3[[1]]$significant),54)
  expect_identical(result3[[2]]$intersectN,c(FALSE,TRUE))
  
})

