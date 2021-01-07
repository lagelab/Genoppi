context('id_significant_proteins')

# read in test data
df <- read_input("data/test.data.txt", sep="\t")$data
statsDf <- calc_mod_ttest(df)

test_that('id_significant_proteins return correct enriched proteins',{

  # a few different test cases
  result <- id_significant_proteins(statsDf, 
	logfc_dir=NULL,logfc_cutoff=NULL,p_cutoff=NULL,fdr_cutoff=0.1) # bidirectional, FDR<=0.1
  expect_equal(sum(result$significant),71)  

  result <- id_significant_proteins(statsDf, 
	logfc_dir="positive",logfc_cutoff=NULL,p_cutoff=0.05,fdr_cutoff=NULL) # logFC>0, p<0.05
  expect_equal(sum(result$significant),57)

  result <- id_significant_proteins(statsDf, 
	logfc_dir="negative",logfc_cutoff=-5,p_cutoff=NULL,fdr_cutoff=0.1) # logFC<-5, FDR<=0.1
  expect_equal(sum(result$significant),16)  
 
})

test_that('function fails when required column is not provided',{

    expect_error(id_significant_proteins("BAD_INPUT"))
  expect_error(id_significant_proteins(df, logfc_dir="positive",logfc_cutoff=NULL,p_cutoff=NULL,fdr_cutoff=NULL))  
  expect_error(id_significant_proteins(df, logfc_dir=NULL,logfc_cutoff=NULL,p_cutoff=NULL,fdr_cutoff=0.1))
  expect_error(id_significant_proteins(df, logfc_dir=NULL,logfc_cutoff=NULL,p_cutoff=0.1,fdr_cutoff=NULL))
  expect_error(id_significant_proteins(df, logfc_dir=NULL,logfc_cutoff=0.1,p_cutoff=NULL,fdr_cutoff=NULL))

})


test_that('function fails when required column is not provided',{
  
  tmp <- statsDf 
  
  expect_error(id_significant_proteins("BAD_INPUT"))
  tmp1 <- tmp; tmp1$logFC <- NULL
  expect_error(id_significant_proteins(tmp1, logfc_dir="positive"))  
  expect_error(id_significant_proteins(tmp1, logfc_dir="positive", logfc_cutoff = 0.1)) 
  tmp2 <- tmp; tmp2$pvalue <- NULL
  expect_error(id_significant_proteins(tmp2, p_cutoff = 0.1))  
  tmp3 <- tmp; tmp3$FDR <- NULL
  expect_error(id_significant_proteins(tmp3, fdr_cutoff = 0.1))  
  
  
})
