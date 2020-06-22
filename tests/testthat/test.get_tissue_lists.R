context('get_tissue_lists')

# test HPA Tissue and genes
tissue <- c("blood","esophagus")
genes <- c('ANXA1', 'EEF1G', 'SNCA', 'WARS')
genes_null <- c("TEST1","TEST2","TEST3")

test_that('get_tissue_lists can return correct data.frame',{
  
  # Multiple tissues can be found
  #result <- get_hpa_lists(tissue, genes)
  #expect_identical(sort(as.character(unique(result$RNA.tissue.specific))),tissue)
  #expect_identical(sort(as.character(unique(result$gene))), genes)
  #expect_equal(result$RNA.tissue.specificity, c("Tissue enhanced", "Tissue enhanced", "Group enriched", "Tissue enhanced"))
  
  # nothing valid is found
  #result2 <- get_gwas_lists(tissue, genes_null)
  #expect_true(is.null(result2))
  
})


