context('impute_na')

df <- data.frame(sample1=rnorm(100),sample2=rnorm(100),sample3=rnorm(100))
df <- rbind(df,data.frame(sample1=rep(NA,10),sample2=rep(NA,10),sample3=rep(NA,10)))

test_that('impute_na can impute missing values correctly',{

  impDf <- impute_na(df,shift=1.5,width=0.2)

  # all missing values should be imputed
  expect_true(sum(is.na(impDf))==0)
  
  # imp columns should flag observed vs. imputed values correctly
  expect_true(sum(impDf[1:100,4:6])==0 & sum(impDf[101:110,4:6])==30)

})

test_that('errors are thrown correctly',{

  expect_error(impute_na(data.frame(df,X=sample(letters,100,replace=T))))
  expect_error(impute_na(df,shift='X',width=0.3))
  expect_error(impute_na(df,shift=1.8,width='Y'))

})

