context('enumerate_replicate_combinations')


test_that('replicates and triplicates',{

 x2 = enumerate_replicate_combinations(2)
 dat2 = data.frame(repA=1,repB=2)
 expect_equal(x2, dat2)
 
 x3 = enumerate_replicate_combinations(3)
 dat3 = data.frame(repA=c(1,1,2),repB=c(2,3,3))
 expect_equal(x3, dat3)
  
})