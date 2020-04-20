context('check_input')

# make artificial data
df1 = data.frame(gene = letters, rep1=rnorm(26), rep2=rnorm(26))
df2 = data.frame(accession_number = letters, rep1=rnorm(26), rep2=rnorm(26), rep3=rnorm(26))
df3 = data.frame(gene = letters, logFC=rnorm(26), pvalue=runif(26), FDR=runif(26))
df4 = data.frame(accession_number = letters, logFC=rnorm(26), pvalue=runif(26), FDR=runif(26))
df5 = data.frame()

test_that('check_input can describe different datasets',{
  
  # gene_rep format
  result = check_input(df1)
  expected_result = list(gene_rep=T, accession_rep=F, gene_signif=F, accession_signif=F)
  expect_identical(result$check,expected_result)
 
  # accession_rep format
  result = check_input(df2)
  expected_result = list(gene_rep=F, accession_rep=T, gene_signif=F, accession_signif=F)
  expect_identical(result$check,expected_result)
  
  # gene_signif format
  result = check_input(df3)
  expected_result = list(gene_rep=F, accession_rep=F, gene_signif=T, accession_signif=F)
  expect_identical(result$check,expected_result)
 
  # accession_signif format
  result = check_input(df4)
  expected_result = list(gene_rep=F, accession_rep=F, gene_signif=F, accession_signif=T)
  expect_identical(result$check,expected_result)

  # bad format
  result = check_input(df5)
  expected_result = list(gene_rep=F, accession_rep=F, gene_signif=F, accession_signif=F)
  expect_identical(result$check,expected_result)
})

