context('calc_fisher')

# read in test data
df <- read_input("data/test.data.txt", sep="\t")$data
statsDf <- calc_mod_ttest(df)
sigDf <- id_enriched_proteins(statsDf)

# InWeb df
inwebDf <- data.frame(listName="InWeb", get_inweb_list("BCL2"))
inwebInterDf <- data.frame(listName="InWeb", intersectN=T)

# gene list df
geneInput <- get_gene_lists("data/test.ALSgenes.txt")
geneDf <- geneInput[[1]]
intersectDf <- geneInput[[2]]

test_that('calc_fisher can return correct overlap results',{

  # InWeb
  result <- calc_fisher(sigDf,inwebDf,inwebInterDf,"BCL2")
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.908")
  expect_true(all(result[[2]][["InWeb"]]$overlap_genes %in% c("LARP1")))

  # gene list
  result <- calc_fisher(sigDf,geneDf,intersectDf,"BCL2")
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.00513")
  expect_true(all(result[[2]][["ALS"]]$overlap_genes %in% c("ATXN2","FUS","PFN1","TAF15")))

})

test_that('errors are reported correctly',{
  
  # InWeb
  expect_warning(calc_fisher(sigDf,inwebDf,inwebInterDf,bait = NULL))
  expect_error(calc_fisher(sigDf,inwebDf,NULL,bait = 'BCL2'))
  
})

