context('plot_scatter_basic')

# read in test data
data <- read_input("data/test.data.txt",header=T,sep="\t")$data
data <- suppressWarnings(calc_mod_ttest(data))
data = id_enriched_proteins(data)

test_that('plot scatter basic single',{
  
  # make single comparison 
  x12 = plot_scatter_basic(data, repA = 'rep1', repB = 'rep2')
  expect_true(!is.null(x12$mapping))
  expect_equal(quo_name(x12$mapping$x), 'rep1')
  expect_equal(quo_name(x12$mapping$y), 'rep2')  
  expect_equal(x12$correlation, 0.6908477)
  
  x13 = plot_scatter_basic(data, repA = 'rep1', repB = 'rep3')
  expect_true(!is.null(x13$mapping))
  expect_equal(quo_name(x13$mapping$x), 'rep1')
  expect_equal(quo_name(x13$mapping$y), 'rep3')  
  
  x23 = plot_scatter_basic(data, repA = 'rep2', repB = 'rep3')
  expect_true(!is.null(x23$mapping))
  expect_equal(quo_name(x23$mapping$x), 'rep2')
  expect_equal(quo_name(x23$mapping$y), 'rep3')  
  
})

test_that('plot scatter basic all',{
  
  # make single comparison 
  res = plot_scatter_basic_all(data)
  expect_true(length(res) == 3)
  expect_equal(quo_name(res$rep1.rep2$ggplot$mapping$x), 'rep1')
  expect_equal(quo_name(res$rep1.rep2$ggplot$mapping$y), 'rep2')  
  expect_equal(res$rep1.rep2$ggplot$correlation, 0.6908477)
  
})

test_that('expected_errors',{
  
  # make single comparison 
  data$rep1 <- as.character(data$rep1)
  expect_error(plot_scatter_basic(data, repA = 'rep1', repB = 'rep2'))
  expect_true(!is.null(plot_scatter_basic(data, repA = 'rep2', repB = 'rep3')))
  data$rep3 <- as.factor(data$rep3)
  expect_error(plot_scatter_basic(data, repA = 'rep2', repB = 'rep3'))
  
})

