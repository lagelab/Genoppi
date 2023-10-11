context('get_pathways')

# test data
genes <- c('FAM167A','SUSD1','PRSS21')

hgncPaths <- c('Serine proteases','Sushi domain containing')

mfPaths <- c('calcium ion binding','protein binding',
  'serine-type peptidase activity','serine-type endopeptidase activity')
ccPaths <- c('membrane','extracellular region','cytoplasm',
  'plasma membrane','side of membrane','extracellular space')
bpPaths <- c('spermatogenesis','proteolysis')

hPaths <- NULL
c1Paths <- c('chr16p13','chr8p23','chr9q32')
c2PathCount <- 24 # many gene sets, just check unique gene set count 
c3PathCount <- 70
c4PathCount <- 3
c5PathCount <- 11
c6PathCount <- 5
c7PathCount <- 157
c8PathCount <- 13

test_that('get_pathways works for all valid database input',{

  # HGNC
  hgnc_out <- get_pathways('hgnc',genes)
  expect_true(all(hgnc_out$pathway %in% hgncPaths))

  # GO MF
  mf_out <- get_pathways('mf',genes)
  expect_true(all(mf_out$pathway %in% mfPaths))

  # GO CC
  cc_out <- get_pathways('cc',genes)
  expect_true(all(cc_out$pathway %in% ccPaths))

  # GO BP
  bp_out <- get_pathways('bp',genes)
  expect_true(all(bp_out$pathway %in% bpPaths))

  # MSigDB H
  h_out <- get_pathways('h',genes)
  expect_true(is.null(h_out))

  # MSigDB C1
  c1_out <- get_pathways('c1',genes)
  expect_true(all(c1_out$pathway %in% c1Paths))

  # MSigDB C2
  c2_out <- get_pathways('c2',genes)
  expect_equal(length(unique(c2_out$pathway)),c2PathCount)

  # MSigDB C3
  c3_out <- get_pathways('c3',genes)
  expect_equal(length(unique(c3_out$pathway)),c3PathCount)

  # MSigDB C4
  c4_out <- get_pathways('c4',genes)
  expect_equal(length(unique(c4_out$pathway)),c4PathCount)
  
  # MSigDB C5
  c5_out <- get_pathways('c5',genes)
  expect_equal(length(unique(c5_out$pathway)),c5PathCount)

  # MSigDB C6
  c6_out <- get_pathways('c6',genes)
  expect_equal(length(unique(c6_out$pathway)),c6PathCount)

  # MSigDB C7
  c7_out <- get_pathways('c7',genes)
  expect_equal(length(unique(c7_out$pathway)),c7PathCount)
 
  # MSigDB C8
  c8_out <- get_pathways('c8',genes)
  expect_equal(length(unique(c8_out$pathway)),c8PathCount)

})

test_that('get_pathways correctly throws errors with invalid database input' ,{

  expect_error(get_pathways('BADNAME',genes),'invalid database input.')
	
  expect_error(get_pathways(123,genes),'invalid database input.')

})
