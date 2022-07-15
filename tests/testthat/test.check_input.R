context('check_input')

# make artificial data
df1 <- data.frame(gene = letters, rep1=rnorm(26), rep2=rnorm(26))
df2 <- data.frame(accession_number = letters, rep1=rnorm(26), rep2=rnorm(26), rep3=rnorm(26))
df3 <- data.frame(gene = letters, logFC=rnorm(26), pvalue=runif(26), FDR=runif(26))
df4 <- data.frame(accession_number = letters, logFC=rnorm(26), pvalue=runif(26), FDR=runif(26))
df5 <- data.frame(gene = letters)
df6a <- data.frame(gene = letters, sample1=rnorm(26), control1=rnorm(26))
df6b <- data.frame(gene = letters, 
                  sample1=rnorm(26), 
                  control1=rnorm(26), control2=rnorm(26))
df6c <- data.frame(gene = letters, 
                  sample1=rnorm(26), sample2=rnorm(26),
                  control1=rnorm(26), control2=rnorm(26))
df7a <- data.frame(accession_number = letters, 
                   sample1=rnorm(26), control1=rnorm(26))
df7b <- data.frame(accession_number = letters,
                  sample1=rnorm(26), sample2=rnorm(26),
                  control1=rnorm(26))
df7c <- data.frame(accession_number = letters,
                  sample1=rnorm(26), sample2=rnorm(26),
                  control1=rnorm(26), control2=rnorm(26), control3=rnorm(26))


test_that('check_input can describe different datasets',{
  
  # gene_rep format
  result <- check_input(df1)
  expected_result <-
    list(
      gene_rep = T,
      gene_sample_control = F,
      gene_signif = F,
      accession_rep = F,
      accession_sample_control = F,
      accession_signif = F
    )
  expect_identical(result$check, expected_result)
  
  # gene_sample_control format
  result <- check_input(df6a)
  expected_result <-
    list(
      gene_rep = F,
      gene_sample_control = F,
      gene_signif = F,
      accession_rep = F,
      accession_sample_control = F,
      accession_signif = F
    )
  expect_identical(result$check, expected_result)
  # gene_sample_control format
  result <- check_input(df6b)
  expect_identical(result$check, expected_result)
  
  
  # gene_sample_control format
  result <- check_input(df6c)
  expected_result <-
    list(
      gene_rep = F,
      gene_sample_control = T,
      gene_signif = F,
      accession_rep = F,
      accession_sample_control = F,
      accession_signif = F
    )
  expect_identical(result$check, expected_result)
  
  # accession_rep format
  result <- check_input(df2)
  expected_result <-
    list(
      gene_rep = F,
      gene_sample_control = F,
      gene_signif = F,
      accession_rep = T,
      accession_sample_control = F,
      accession_signif = F
    )
  expect_identical(result$check, expected_result)
  
  # accession_sample_control format
  result <- check_input(df7a)
  expected_result <-
    list(
      gene_rep = F,
      gene_sample_control = F,
      gene_signif = F,
      accession_rep = F,
      accession_sample_control = F,
      accession_signif = F
    )
  expect_identical(result$check, expected_result)
  # accession_sample_control format
  result <- check_input(df7b)
  expect_identical(result$check, expected_result)
  
  result <- check_input(df7c)
  expected_result <-
    list(
      gene_rep = F,
      gene_sample_control = F,
      gene_signif = F,
      accession_rep = F,
      accession_sample_control = T,
      accession_signif = F
    )
  expect_identical(result$check, expected_result)
  
  # gene_signif format
  result <- check_input(df3)
  expected_result <-
    list(
      gene_rep = F,
      gene_sample_control = F,
      gene_signif = T,
      accession_rep = F,
      accession_sample_control = F,
      accession_signif = F
    )
  expect_identical(result$check, expected_result)
  
  # accession_signif format
  result <- check_input(df4)
  expected_result <-
    list(
      gene_rep = F,
      gene_sample_control = F,
      gene_signif = F,
      accession_rep = F,
      accession_sample_control = F,
      accession_signif = T
    )
  expect_identical(result$check, expected_result)
  
  # bad format
  result <- check_input(df5)
  expected_result <-
    list(
      gene_rep = F, gene_sample_control = F, gene_signif = F,
      accession_rep = F, accession_sample_control = F, accession_signif = F
    )
  expect_identical(result$check, expected_result)

  # empty data frame
  expect_error(check_input(data.frame()))  
  
  # bad input
  expect_error(check_input("BAD_INPUT"))  

})

#test_that('column names are accepted but column content is invalid',{
  
  # expected numeric columns
  #df1a = df1
  #df1a$rep2 = as.character(df1a$rep2)
  #expect_error(check_input(df1a))
  
  # expected character column
  #df1b = df1b
  #df1b$gene = as.numeric(df1b$gene)
  #expect_error(check_input(df1b))
  
  
#})




