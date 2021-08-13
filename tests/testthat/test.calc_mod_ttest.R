context('calc_mod_ttest')

# read in test data
df <- read_input("data/test.data.txt", sep="\t")$data
df_rep_col <- data.frame(gene=df$gene,df[which(grepl('rep[0-9]',names(df)))])
df_sample_control_col <- df[-which(grepl('rep[0-9]', names(df)))]

test_that(
  'calc_mod_ttest two-sample results is the same as that obtained by the workflow below',
  {
    # process test data using two-sample mod t-test in limma directly as sanity
    # check that statistical test is running correctly
    design <- model.matrix(
      ~ factor(c('sample','control','sample','control','sample','control'), 
               levels = c('control', 'sample')))
    fit <- limma::lmFit(df_sample_control_col[-1],
                        design,
                        method = "robust",
                        maxit = 2000)
    fit2 <- limma::eBayes(fit)
    mod.ttest <- limma::topTable(fit2,
                                 number = nrow(fit2),
                                 sort.by = 'none')
    df_test <- cbind(df_sample_control_col,
                       mod.ttest[, c("logFC", "P.Value", "adj.P.Val")])
    df_test <- df_test[with(df_test, order(-logFC, adj.P.Val)), ]
    
    df_result <-
      calc_mod_ttest(
        df_sample_control_col,
        iter = 2000,
        order = T,
        two_sample = T
      )
    
    expect_equal(
      object = df_result$logFC,
      expected = df_test$logFC,
      tolerance = 0.00001
    )
    expect_equal(
      object = df_result$pvalue,
      expected = df_test$P.Value,
      tolerance = 0.00001
    )
    expect_equal(
      object = df_result$FDR,
      expected = df_test$adj.P.Val,
      tolerance = 0.00001
    )
})


test_that(
  'calc_mod_ttest handles data frame with rep columns correctly',
  {
    result <- calc_mod_ttest(df_rep_col) 
    expected_cols <-
      c(
        "gene",
        "rep1", "rep2", "rep3",
        "logFC", "pvalue", "FDR"
      )
    expect_identical(colnames(result),expected_cols)
    
    # testing message for input with rep columns only and no parameter for
    # two-sample
    expect_message(
      calc_mod_ttest(df_rep_col),
      'Type of moderated t-test not specified \\(defaulting to one-sample mod t-test\\).'
    )
    # testing message for input with rep columns only and two-sample parameter is
    # FALSE
    expect_message(calc_mod_ttest(df_rep_col, two_sample = FALSE),
                   'Using one-sample mod t-test.')
})

test_that(
  'calc_mod_ttest handles data frame with sample and control columns correctly',
  {
    result <- calc_mod_ttest(df_sample_control_col) 
    expected_cols <-
      c(
        "gene",
        "sample1", "sample2", "sample3",
        "control1", "control2", "control3",
        "logFC", "pvalue", "FDR"
      )
    expect_true(all(colnames(result) %in% expected_cols))
    
    expect_message(
      calc_mod_ttest(df_sample_control_col),
      'Type of moderated t-test not specified \\(defaulting to two-sample mod t-test\\).'
    )
    
    expect_message(calc_mod_ttest(df_sample_control_col, 
                                  two_sample = TRUE),
                   'Using two-sample mod t-test.')
  }
)

test_that(
  'calc_mod_ttest handles data frame with all columns correctly',
  {
    result <- calc_mod_ttest(df) 
    expected_cols <-
      c(
        "gene",
        "sample1", "sample2", "sample3",
        "control1", "control2", "control3",
        "rep1", "rep2", "rep3",
        "logFC", "pvalue", "FDR"
      )
    expect_true(all(colnames(result) %in% expected_cols))
    
    expect_message(
      calc_mod_ttest(df),
      'Type of moderated t-test not specified \\(defaulting to two-sample mod t-test\\).'
    )
    
    expect_message(calc_mod_ttest(df, 
                                  two_sample = FALSE),
                   'Using one-sample mod t-test.')
    
    expect_message(calc_mod_ttest(df, 
                                  two_sample = TRUE),
                   'Using two-sample mod t-test.')
  }
)

test_that(
  'calc_mod_ttest throws appropriate error when insufficient is provided',
  {
    # testing message for input with rep columns only and two-sample parameter
    # is FALSE
    expect_error(
      calc_mod_ttest(df_rep_col, two_sample = TRUE),
      'trying to use two-sample moderated t-test in calc_mod_ttest but less than 2 columns of sample or control columns is provided.'
    )
    
    expect_error(
      calc_mod_ttest(df_sample_control_col, two_sample = FALSE),
      'trying to use one-sample moderated t-test in calc_mod_ttest but less than 2 rep columns is provided.'
    )
    
    expect_error(
      calc_mod_ttest(df["gene"]),
      'data does not contain enough rep or sample and control columns! LmFit failed!'
    )
    
    expect_error(
      calc_mod_ttest(df_sample_control_col[c("gene", "sample1", "control2")]),
      'data does not contain enough rep or sample and control columns! LmFit failed!'
    )
    
    expect_error(
      calc_mod_ttest(df_sample_control_col[c("gene", "sample1", "sample2")]),
      'data does not contain enough rep or sample and control columns! LmFit failed!'
    )
    
    expect_error(
      calc_mod_ttest(df_sample_control_col[c("gene", "sample1", "sample2", "control2")]), 
      'data does not contain enough rep or sample and control columns! LmFit failed!'
    )
    
    expect_error(
      calc_mod_ttest(df_sample_control_col[c("gene", "sample1", "control2")],
                     two_sample = TRUE),
      'trying to use two-sample moderated t-test in calc_mod_ttest but less than 2 columns of sample or control columns is provided.'
    )
    
    expect_error(
      calc_mod_ttest(df_sample_control_col[c("gene", "sample1", "sample2")],
                     two_sample = TRUE),
      'trying to use two-sample moderated t-test in calc_mod_ttest but less than 2 columns of sample or control columns is provided.'
    )
    
    expect_error(
      calc_mod_ttest(
        df_sample_control_col[c("gene", "sample1", "sample2", "control2")], 
        two_sample = TRUE),
      'trying to use two-sample moderated t-test in calc_mod_ttest but less than 2 columns of sample or control columns is provided.'
    )
    
    expect_error(
      calc_mod_ttest(df[c("gene", "rep1")]), 
      'data does not contain enough rep or sample and control columns! LmFit failed!'
    )
    
    expect_error(
      calc_mod_ttest(df[c("gene", "rep2")], two_sample = FALSE), 
      'trying to use one-sample moderated t-test in calc_mod_ttest but less than 2 rep columns is provided.'
    )
  })


