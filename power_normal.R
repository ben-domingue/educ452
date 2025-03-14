pval<-function(N,mu=1,sigma=1) {
    x<-rnorm(N,mean=mu,sd=sigma)
    m<-mean(x)
    se<-sd(x)/sqrt(length(x))
    z<-(m-0.5)/se
    pnorm(abs(z),lower.tail=FALSE)
}
pval<-Vectorize(pval)

N<-sort(runif(1000,10,100))
p<-pval(N)

par(mgp=c(2,1,0))
plot(NULL,xlim=range(N),ylim=0:1,xlab="N draws",ylab="Proportion of times that I reject the null")
f<-function(p,N,...) {
    x<-ifelse(p<.05,1,0)
    m<-loess(x~N)
    lines(N,fitted(m),...)
}
f(p,N,lwd=2,col='black')
