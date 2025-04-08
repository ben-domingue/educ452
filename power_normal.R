##null hypothesis: x comes from N(mean=0.5,sd=1)

##first, a function to compute pvalue:
pval<-function(N,mu=1,sigma=1) {
    x<-rnorm(N,mean=mu,sd=sigma)
    m<-mean(x)
    se<-1/sqrt(length(x))  ##the 1 here is due to the assumption in the null hypothesis!! we would use a 'plug-in' estimator for this quantity in some cases.
    z<-(m-0.5)/se ##0.5 here due to null hypothesis
    2*pnorm(abs(z),lower.tail=FALSE) ##two-tailed test
}
pval<-Vectorize(pval) #this just makes sure that pval() will work for vector-valued N so that i can do it for lots of values

##now we'll iterate over many N values
N<-sort(runif(10000,10,100))
p<-pval(N)

##and finally a power curve
par(mgp=c(2,1,0))
plot(NULL,xlim=range(N),ylim=0:1,xlab="N draws",ylab="Proportion of times that I reject the null")
f<-function(p,N,...) {
    x<-ifelse(p<.05,1,0)
    m<-loess(x~N)
    lines(N,fitted(m),...)
}
f(p,N,lwd=2,col='black')

