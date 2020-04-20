context('get_pathways')

# test data
genes <- c("FAM167A","SUSD1","PRSS21")

hgncPaths <- c("Antisense RNAs","Serine proteases","Sushi domain containing")

mfPaths <- c("serine-type endopeptidase activity","calcium ion binding",
  "protein binding","serine-type peptidase activity")
ccPaths <- c("extracellular region","extracellular space","cytoplasm","plasma membrane",
  "membrane","integral component of membrane","anchored component of membrane")
bpPaths <- c("proteolysis","spermatogenesis")

hPaths <- NULL
c1Paths <- c("chr8p23","chr9q32","chr16p13")
c2PathCount <- 20 # too many gene sets, just check unique gene set count 
c3PathCount <- 73
c4PathCount <- 3
c5PathCount <- 13
c6PathCount <- 5
c7PathCount <- 146

test_that('get_pathways can return correct data.frame',{

  # HGNC
  hgnc_out <- get_pathways("hgnc",genes)
  expect_true(all(hgnc_out$pathway %in% hgncPaths))

  # GO MF
  mf_out <- get_pathways("mf",genes)
  expect_true(all(mf_out$pathway %in% mfPaths))

  # GO CC
  cc_out <- get_pathways("cc",genes)
  expect_true(all(cc_out$pathway %in% ccPaths))

  # GO BP
  bp_out <- get_pathways("bp",genes)
  expect_true(all(bp_out$pathway %in% bpPaths))

  # MSigDB H
  h_out <- get_pathways("h",genes)
  expect_true(is.null(h_out))

  # MSigDB C1
  c1_out <- get_pathways("c1",genes)
  expect_true(all(c1_out$pathway %in% c1Paths))

  # MSigDB C2
  c2_out <- get_pathways("c2",genes)
  expect_equal(length(unique(c2_out$pathway)),c2PathCount)

  # MSigDB C3
  c3_out <- get_pathways("c3",genes)
  expect_equal(length(unique(c3_out$pathway)),c3PathCount)

  # MSigDB C4
  c4_out <- get_pathways("c4",genes)
  expect_equal(length(unique(c4_out$pathway)),c4PathCount)
  
  # MSigDB C5
  c5_out <- get_pathways("c5",genes)
  expect_equal(length(unique(c5_out$pathway)),c5PathCount)

  # MSigDB C6
  c6_out <- get_pathways("c6",genes)
  expect_equal(length(unique(c6_out$pathway)),c6PathCount)

  # MSigDB C7
  c7_out <- get_pathways("c7",genes)
  expect_equal(length(unique(c7_out$pathway)),c7PathCount)

})
