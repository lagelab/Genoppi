context('calc_mod_ttest')

# read in test data
df <- read_input("data/test.data.txt",header=T,sep="\t")$data

test_that('calc_mod_ttest can return results in data.frame',{

  result <- calc_mod_ttest(df) 
  expected_cols <- c("gene","rep1","rep2","rep3","logFC","pvalue","FDR")
  expect_identical(colnames(result),expected_cols)
 
})
