
############################################################################################################
##a. the CLT, https://en.wikipedia.org/wiki/Central_limit_theorem
#Which is relevant, N or n, in terms of having the CLT apply?
fun<-function(N,n,p=0.5) { #N is the number of people, n is the number of tosses per person
    x<-rbinom(N,n,p)
    x<-(x-mean(x))/sd(x)
    hist(x,main='',freq=FALSE,yaxt='n')
    xv<-seq(-3,3,length.out=1000)
    lines(xv,dnorm(xv))
    legend("topleft",bty='n',c(paste0("N=",N),paste0("n=",n)))
}
par(mfrow=c(5,5),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(0,4))
for (N in c(10,50,100,1000,5000)) for (n in c(5,25,100,1000,5000)) fun(N,n)
