library(BETR)
rata_S001=data.frame(subject=rep("S1",9),"log_concentration"=1:9,intensity=c(4, 6 , 7 , 8  ,5  ,9 , 9, 10,10))
res=ibt(df=rata_S001,maxConc=10,minConc=0)
test_that("test IBT", expect_true(res[1]==1.5))


rata_S001[,"intensity"]=c(4, 10 , 7 , 8  ,5  ,9 , 9, 10,10)
res=ibt(rata_S001,maxConc=10,minConc=0)
test_that("test IBT", expect_true(res[1]==1.5))

rata_S001[,"intensity"]=c(4, 5 , 4 , 8  ,5  ,9 , 9, 10,19)
res=ibt(rata_S001,maxConc=10,minConc=0)
test_that("test IBT", expect_true(res[1]==7.5))

rata_S001[,"intensity"]=c(3,1,0,0,1,0,1,2,3)
res=ibt(rata_S001,maxConc=10,minConc=0)
test_that("test IBT", expect_true(res[1]==6.5))

data(astringency)
res_1=ibt(astringency,minConc=0,maxConc=10)

data(astringency)
res_2=ibt(astringency)
test_that("test IBT", expect_true(
  all(res_1==res_2)))
