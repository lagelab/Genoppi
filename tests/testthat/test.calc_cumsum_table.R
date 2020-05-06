context('calc_cumsum_table')

# get the data
df = data.frame(pathways = c('a','b','c','d','e','f','g','h','i'), 
                Freq = c(50,20,10,5,2,1,1,1,1))

test_that('basic functionality',{

  result = calc_cumsum_table(df, col.id = 'pathways')
  expect_equal(result$n, c(1,2,3,4,5,9,9,9,9))
  
})

test_that('expected errors and warnings',{
  
  expect_error(calc_cumsum_table(df, col.id = 'invalid column'))
  expect_error(calc_cumsum_table(df, col.freq = 'invalid column'))
  
})


