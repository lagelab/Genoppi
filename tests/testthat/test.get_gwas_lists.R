context('get_gwas_lists')

# test GWAS traits
traits <- c("Amyotrophic lateral sclerosis","Amyotrophic lateral sclerosis (sporadic)")
genes <- as.factor(c("SUSD1","LIPC","TEST1","FAM167A","DACH1","C9orf72"))
genes_null <- as.factor(c("TEST1","TEST2","TEST3"))

test_that('get_gwas_lists can return correct data.frame',{

  # GWAS SNPs overlap with genes (include catalog entries contaning multiple SNPs)
  result <- get_gwas_lists(traits, genes)
  expect_identical(sort(as.character(unique(result$DISEASE.TRAIT))),traits)
  expect_identical(sort(as.character(unique(result$gene))),c("C9orf72","DACH1","LIPC","SUSD1"))
  expect_equal(sort(unique(result$PUBMEDID)),
	c(17362836,18084291,19734901,20801717,20801718,22959728,24256812,
		24529757,24931836,25442119,27455348,28931804,29566793,30976013))

  # GWAS SNPs do not overlap with gense
  result2 <- get_gwas_lists(traits, genes_null)
  expect_true(is.null(result2))

})
