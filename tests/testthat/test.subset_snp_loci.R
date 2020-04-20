context('susbset_snp_loci')

# test SNP-gene data.frame
df <- data.frame(SNP=c("SNP1","SNP2","SNP3","SNP4","SNP1","SNP4","SNP1"),
        gene=c("GeneA","GeneA","GeneB","GeneC","GeneD","GeneE","GeneF"))

singleDf <- data.frame(SNP=c("SNP2","SNP3"),gene=c("GeneA","GeneB"))
multiDf <- data.frame(SNP=c("SNP1","SNP4","SNP1","SNP4","SNP1"),
        gene=c("GeneA","GeneC","GeneD","GeneE","GeneF"))

test_that('subset_snp_loci can reuturn singleGeneDf and multiGeneDf',{

  # df contains both single and multi-gene loci
  result <- subset_snp_loci(df)
  expect_equal(result$singleGeneDf,singleDf)
  expect_equal(result$multiGeneDf,multiDf)

  # df contains only single-gene loci
  result2 <- subset_snp_loci(singleDf)
  expect_equal(result2$singleGeneDf,singleDf)
  expect_true(is.null(result2$multiGeneDf))

  # df contains only multi-gene loci
  result3 <- subset_snp_loci(multiDf)
  expect_true(is.null(result3$singleGeneDf))
  expect_equal(result3$multiGeneDf,multiDf)

})
