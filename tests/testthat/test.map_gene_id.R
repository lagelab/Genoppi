context('map_gene_id')

test_that('map_gene_id correctly maps UniProt acession_number to gene name' ,{
 
  # read in test data
  df <- head(read.table(file='data/BCL2vsIgG.txt', header=T))
  # to test accession contaiing isoform suffix
  df$accession_number[1] <- paste(df$accession_number[1],'3',sep='-')
  df$accession_number[2] <- paste(df$accession_number[2],'2',sep='.')
  # to test unmappable accession
  df$accession_number[3] <- 'BADNAME'
  df$gene[3] <- NA

  result <- map_gene_id(dplyr::select(df,!gene))
  result <- dplyr::relocate(result,gene,.after=accession_number)
  expect_equal(result, df)
  
})


test_that('map_gene_id correctly throws errors and warnings' ,{
 
  # read in test data
  df <- head(read.table(file='data/BCL2vsIgG.txt', header=T))

  # throws error if input has no accession_number column
  expect_error(map_gene_id(dplyr::select(df,!accession_number)),
    '"accession_number" column not found in input.')

  # throws warning if input already has gene column
  expect_warning(map_gene_id(df),
    '"gene" column found in input, would be replaced in output.')
  
})

