############################################################################################################
##A. bernoulli distribution
##a. how good of an estimate of p is the mean? interplay between the magnitude of p and the sample size

p.est<-list()
for (N in c(5,10,50,100,1000)) {
    for (p in seq(0.5,0.99,length.out=25)) {
        m<-numeric()
        for (i in 1:1000) {
            coins<-rbinom(N,1,p)
            m[i]<-mean(coins)
        }
        p.est[[paste(N,p)]]<-c(N,p,mean((m-p)^2))
    }
}

x<-data.frame(do.call("rbind",p.est))
L<-split(x,x[,1])
cols<-c("pink","red","blue","gray","black")
plot(NULL,ylim=c(0,0.1),xlim=range(x[,2]),xlab='p',ylab='mse')
for (i in 1:length(L)) lines(L[[i]][,-1],lwd=2,col=cols[i])
legend("topright",bty='n',fill=cols,names(L))

############################################################################################################
##b. the binomial distribution
ntrial<-15
x<-rbinom(10000,ntrial,.5)
y<-numeric()
for (i in 1:10000) {
    z<-numeric()
    for (j in 1:ntrial) z[j]<-rbinom(1,1,.5)
    y[i]<-sum(z)
}
x<-factor(x,levels=0:ntrial)
y<-factor(y,levels=0:ntrial)
plot(as.numeric(table(x)),as.numeric(table(y))); abline(0,1)

############################################################################################################
##c. the CLT, https://en.wikipedia.org/wiki/Central_limit_theorem
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
