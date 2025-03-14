sim<-function(N,p=.6) z<-rbinom(N,1,p)
sim<-Vectorize(sim,'N')

##normal approximation
norm<-function(x) {
    m<-mean(x)
    se<-sd(x)/sqrt(length(x))
    z<-(m-0.5)/se
    pnorm(abs(z),lower.tail=FALSE)
}

##exact binomial
bin<-function(x) {
    s<-sum(x)
    if (s<(length(x)/2)) s<-length(x)-s
    p<-1-pbinom(s,length(x),.5)
    p
}

N<-sort(runif(10000,10,500))
x<-sim(N)
p1<-sapply(x,norm)
p2<-sapply(x,bin)

##Let's first assess p-values
plot(N,p1-p2)

par(mgp=c(2,1,0))
plot(NULL,xlim=range(N),ylim=0:1,xlab="# tosses",ylab="Proportion of times that I reject the null")
f<-function(p,N,...) {
    x<-ifelse(p<.05,1,0)
    m<-loess(x~N)
    lines(N,fitted(m),...)
}
f(p1,N,lwd=2,col='black')
f(p2,N,lwd=2,col='red')
legend("bottomright",bty='n',fill=c("black","red"),c("Normal","Exact"))
