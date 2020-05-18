context('misc')

test_that('various misc functions',{
  
  # minor funcs
  expect_equal(lun(c(1,2,3,1)), 3)
  expect_equal(c('a','q') %nin% letters, !c('a','q') %in% letters)
  expect_equal(null_omit(list(x=NULL,y=1,Z=NULL)), list(y=1))
  expect_equal(as.bait('BCL2'), list(bait=data.frame(gene='BCL2',col_significant='red',col_other='orange')))
  expect_equal(italics('BCL2'), '<i>BCL2</i>')
  expect_output(catf('hi',file=stdout()),'hi')
  
  # assigning frequencies
  orig = assign_freq(data.frame(x=c('a','b','c','d','a','a')), col = 'x')
  ref = data.frame(x=c('a','a','a','b','c','d'), Freq=c(3,3,3,1,1,1))
  expect_equal(orig,ref)
  
  # colors
  expect_equal(color_gradient(c(1,2,3,4,5,6),colors = c('red','green'),colsteps = 6),c("#FF0000","#CC3300","#996600","#659900","#32CC00","#00FF00"))
  expect_equal(head(color_distinct()), c("#7FC97F", "#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F"))
  

})

test_that('assign_color',{
  
  # standard use
  result = assign_color(data.frame(x=c('a','b','c','d','a','a')), col = 'x')
  expect_equal(result$color, c("#7FC97F", "#7FC97F", "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99"))
  
  #result = assign_color(data.frame(x=c('a','b','c','d','a','a')), col = 'x', order_by_freq = F)
  #expect_equal(result$color, c("#7FC97F", "#7FC97F", "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99"))
  
})

test_that('translate plotly symbols and ggplot shapes',{
  
  x = table_symbols()
  expect_equal(lun(x$shape), length(x$shape))
  expect_equal(lun(x$symbol), length(x$symbol))
  expect_equal(symbol_to_shape(x$symbol), x$shape)
  expect_equal(shape_to_symbol(x$shape), x$symbol)
  expect_error(symbol_to_shape(x$shape))
  expect_error(shape_to_symbol(x$symbol))
  
})

test_that('is_col working as expected',{
  
  #df = data.frame(gene=letters, rep1=runif(26), rep2=runif(26))
  #expect_true(is_cols(df, 'rep1', is.numeric))
  #expect_true(is_cols(df, 'rep[0-9]', is.numeric))
  #expect_false(is_cols(df, 'rep1', is.character))
  #expect_true(is_cols(df, 'gene', is.factor))
  #expect_error(is_cols(df, 'fwafa', is.factor))
  
})



