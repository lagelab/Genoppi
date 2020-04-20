context('read_input')

# make artificial data
df1 = data.table::data.table(gene=letters, rep1=rnorm(26), rep2=rnorm(26), rep3=rnorm(26))
tmp = tempfile()
write.table(df1,tmp,quote=F,sep="\t",row.names=F)

test_that('read_input can return input data.frame and data format',{
 
  # check returned data.frame 
  result = read_input(tmp,sep="\t",header=T)
  expect_equal(result$data,df1)
 
  # check returned data format list
  expected_check = list(gene_rep=T, accession_rep=F, gene_signif=F, accession_signif=F)
  expect_identical(result$format$check,expected_check)
 
})

