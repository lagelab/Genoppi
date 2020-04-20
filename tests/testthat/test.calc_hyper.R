context('calc_hyper')

# read in test data
df <- read_input("data/test.data.txt",header=T,sep="\t")$data
statsDf <- calc_mod_ttest(df)
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
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.175")
  expect_true(all(result[[2]][["InWeb"]]$successInSample_genes %in% c("HSP90AA1","BAX","RPL8","RPLP2")))

  # gene list
  result <- calc_hyper(sigDf,geneDf,intersectDf,"BCL2")
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.567")
  expect_true(result[[2]][["ALS"]]$successInSample_genes %in% c("HNRNPA2B1"))

})

test_that('errors are reported correctly',{

  expect_warning(calc_hyper(sigDf,inwebDf,inwebInterDf,bait = NULL))
  expect_error(calc_hyper(sigDf,inwebDf,NULL,bait = 'BCL2'))
  
})

