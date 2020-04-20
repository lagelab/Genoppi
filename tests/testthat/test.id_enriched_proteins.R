context('id_enriched_proteins')

# read in test data
df <- read_input("data/test.data.txt",header=T,sep="\t")$data
statsDf <- calc_mod_ttest(df)

test_that('id_enriched_proteins return correct enriched proteins',{

  # a few different test cases
  result <- id_enriched_proteins(statsDf, 
	logfc_dir=NULL,logfc_cutoff=NULL,p_cutoff=NULL,fdr_cutoff=0.1) # bidirectional, FDR<=0.1
  expect_equal(sum(result$significant),71)  

  result <- id_enriched_proteins(statsDf, 
	logfc_dir="positive",logfc_cutoff=NULL,p_cutoff=0.05,fdr_cutoff=NULL) # logFC>0, p<0.05
  expect_equal(sum(result$significant),56)

  result <- id_enriched_proteins(statsDf, 
	logfc_dir="negative",logfc_cutoff=-5,p_cutoff=NULL,fdr_cutoff=0.1) # logFC<-5, FDR<=0.1
  expect_equal(sum(result$significant),16)  
 
})

test_that('function fails when no mapping is not provided',{
  
  expect_error(id_enriched_proteins(df, logfc_dir=NULL,logfc_cutoff=NULL,p_cutoff=NULL,fdr_cutoff=0.1))
  expect_error(id_enriched_proteins(df, logfc_dir=NULL,logfc_cutoff=NULL,p_cutoff=0.1,fdr_cutoff=NULL))
  expect_error(id_enriched_proteins(df, logfc_dir=NULL,logfc_cutoff=0.1,p_cutoff=NULL,fdr_cutoff=NULL))

})
