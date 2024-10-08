library(BETR)
bet_S001=data.frame(subject=rep("S1",9),"log_concentration"=1:9,"AFC"=c(1, 1 , 1 , 1  ,1  ,1 , 1, 1,1),"intensity"=c(2,1,5,4,7,8,9,10,10))
graph(df=bet_S001)

data(astringency)
graph(astringency,subject="S002")
