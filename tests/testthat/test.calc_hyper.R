context('calc_hyper')

# read in test data (BCL2 vs. IgG in A375)
df <- read_input("data/test.data2.txt", sep="\t")$data
statsDf <- suppressWarnings(calc_mod_ttest(df))
sigDf <- id_enriched_proteins(statsDf)

# InWeb df
inwebDf <- data.frame(listName="InWeb", get_inweb_list("BCL2"))
inwebInterDf <- data.frame(listName="InWeb",intersectN=T)

# gene list df
geneInput <- get_gene_lists("data/test.ALSgenes.txt")
geneDf <- geneInput[[1]]
intersectDf <- geneInput[[2]]

test_that('calc_hyper can return correct overlap results',{
  
  # InWeb
  result <- calc_hyper(sigDf,inwebDf,inwebInterDf,"BCL2")
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.598")
  expect_true(all(result[[2]][["InWeb"]]$successInSample_genes %in% c("BAX","DYNLL1","HNRNPD")))

  # gene list
  result <- calc_hyper(sigDf,geneDf,intersectDf,"BCL2")
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.833")
  expect_true(all(result[[2]][["ALS"]]$successInSample_genes %in% c("ATXN2","MATR3")))

})

test_that('errors are reported correctly',{

  expect_warning(calc_hyper(sigDf,inwebDf,inwebInterDf,bait = NULL))
  expect_error(calc_hyper(sigDf,inwebDf,NULL,bait = 'BCL2'))
  
})

