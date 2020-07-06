context('parse_rd')

# get data files to be documented
data.files = tools::file_path_sans_ext(list.files('data/', full.names = F))
data.documentation = file.path('man//',paste0(data.files, '.Rd'))

test_that('documentation can be parsed into html',{

  # run documentation
  dat = lapply(data.documentation, function(file) genoppi:::parse_rd(readLines(file), what = c("title", "description", "source", "references"), combine.with = ' '))
  names(dat) <- data.documentation
  
  # check that no nulls are present
  expect_equal(dat$`man///gtex_table.Rd`$title, "GTEx tissue specificity annotations")
  expect_false(any(unlist(lapply(dat, function(x) any(is.null(x))))))
  
})



