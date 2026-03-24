f<-function(N,r) {
    library(MASS)
    x<-mvrnorm(N,mu=rep(0,2),Sigma=matrix(c(1,r,r,1),2,2))
    m<-lm(x[,2]~x[,1])
    p<-summary(m)$coef[2,4]
    c(r,p)
}

N<-sort(runif(5000,min=1,max=4))
L<-list()
library(parallel)
for (r in seq(0,.5,by=.1)) { ##r is correlation
    x<-mclapply(round(10^N),f,r,mc.cores=10)
    L[[as.character(r)]]<-do.call("rbind",x)
}


par(mgp=c(2,1,0))
plot(NULL,xlim=c(1,4),ylim=0:1,xlab="N",ylab="Proportion of times that I reject the null",xaxt='n')
axis(side=1,at=1:4,labels=10^(1:4))
abline(h=.8,col='gray')
cols<-colorRampPalette(c("blue", "red"))( length(L))
tab<-list()
for (i in 1:length(L)) {
    x<-L[[i]]
    m0<-loess(x[,2]<.05 ~ N)
    lines(N,m0$fitted,lty=1,col=cols[i])
}
legend("right",bty='n',legend=names(L),fill=cols,title="Correlation")
