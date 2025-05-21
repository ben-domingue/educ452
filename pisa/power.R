## simulating data from the Rasch model
simest<-function(np,ni=25) { 
    rmse<-function(x,y) sqrt(mean((x-y)^2))
    th<-rnorm(np)
    b<-rnorm(ni)
    k<-outer(th,b,'-')
    p<-1/(1+exp(-k))
    resp<-p
    for (i in 1:ncol(resp)) resp[,i]<-rbinom(np,1,p[,i])
    resp<-data.frame(resp)
    m1<-mirt::mirt(resp,1,'Rasch')
    b.est<-coef(m1,simplify=TRUE,IRTpars=TRUE)$item[,2]
    rmse(b,b.est)
}

N<-10^(runif(25,2,4))
N<-sort(round(N)) #sorting is handy for visualization
library(parallel)
nc<-10 ##you will need to set this for your machine
z<-mclapply(N,simest,mc.cores=nc)

err<-unlist(z)
plot(N,err,pch=19,col='gray',cex=.5,ylim=c(0,max(err)))
m<-loess(err~N)
lines(N,m$fitted)
