context('draw_genoppi_venn')

# suppress venn diagram logs
futile.logger::flog.threshold(futile.logger::ERROR)

test_that('venn diagram with double comparison',{
  
  # setup data
  double_venn = list(A=letters[1:10],
                     B=letters[8:15])
  
  # test double venn values
  output = draw_genoppi_venn(double_venn)
  expect_equal(as.numeric(output[[5]]$label), length(double_venn$A) - length(intersect(double_venn$A, double_venn$B)))
  expect_equal(as.numeric(output[[6]]$label), length(double_venn$B) - length(intersect(double_venn$A, double_venn$B)))
  expect_equal(as.numeric(output[[7]]$label), length(intersect(double_venn$A, double_venn$B)))
  
})


test_that('venn diagram with triple comparison',{
  
  # setup data
  triple_venn = list(A=letters[1:10],
       B=letters[8:15],
       C=c('h',letters[18:23]))
  
  # test triple venn
  output = draw_genoppi_venn(triple_venn)
  expect_equal(as.numeric(output[[11]]$label), length(intersect(intersect(triple_venn$A, triple_venn$B), triple_venn$C)))
  
})

test_that('check that the venn diagram can be drawn',{
  
  # double
  double_venn = list(A=letters[1:10],B=letters[8:15])
  output = draw_genoppi_venn(double_venn)
  plot_venn(output, scale = 0.4)
  
  # triple
  triple_venn = list(A=letters[1:10],
                     B=letters[8:15],
                     C=c('h',letters[18:23]))
  output = draw_genoppi_venn(triple_venn, margin = 0)
  plot_venn(output, scale = 0.9)
  expect_true(!is.null(output)) # expect all these lines to run without error
  
  
})


test_that('check errors',{
  
  # a venn diagram needs at least a vector of length 2
  single_venn = list(A=letters[1:10])
  expect_error(draw_genoppi_venn(single_venn))

})


