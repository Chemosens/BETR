
library(BETR)

bet_S001=data.frame(subject=rep("S1",9),"log_concentration"=1:9,"AFC"=c(1, 1 , 1 , 1  ,1  ,1 , 1, 1,1))
res=bet(df=bet_S001)
test_that("test 1", expect_true(res[1]==0.5))

bet_S002=data.frame(subject=rep("S1",9),"log_concentration"=1:9,"AFC"=c(0, 1 , 1 , 1  ,1  ,1 , 1, 1,1))
res2=bet(df=bet_S002)
test_that("test 2", expect_true(res2[1]==1.5))

bet_S003=data.frame(subject=rep("S1",9),"log_concentration"=1:9,"AFC"=c(1, 1 , 1 , 1  ,1  ,1 , 1, 1,0))
res3=bet(df=bet_S003)
test_that("test 3", expect_true(res3[1]==9.5))

bet_S004=data.frame(subject=rep("S1",9),"log_concentration"=1:9,"AFC"=c(1, 1 , 0 , 1  ,0  ,1 , 0, 0,1))
res4=bet(df=bet_S004)
test_that("test 4", expect_true(res4[1]==8.5))
data(astringency)
bet(astringency)
