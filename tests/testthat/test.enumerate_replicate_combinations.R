context('enumerate_replicate_combinations')


test_that('replicates and triplicates',{
 x2_df <- data.frame(rep1=1,rep2=2)
 x2 = enumerate_replicate_combinations(x2_df)
 dat2 = c("rep1.rep2")
 expect_equal(x2, dat2)
 
 x3_df <- data.frame(rep1=1,rep2=2,rep3=3)
 x3 = enumerate_replicate_combinations(x3_df)
 dat3 = c("rep1.rep2", "rep1.rep3", "rep2.rep3")
 expect_equal(x3, dat3)
  
})