pbinom(6-1,10,.5,lower.tail=FALSE)
pbinom(60-1,100,.5,lower.tail=FALSE)
pbinom(600-1,1000,.5,lower.tail=FALSE)

#########################################################
f<-function(N,bias=.5) {
    x<-rbinom(N,1,bias)
    2*pbinom(sum(x)-1,length(x),.5,lower.tail=ifelse(mean(x)>.5,FALSE,TRUE))
}

set.seed(10101)
N<-100
x<-rbinom(N,1,.57)
sum(x)
2*pbinom(sum(x)-1,length(x),.5,lower.tail=FALSE)
N<-100
x<-rbinom(N,1,.57)
sum(x)
2*pbinom(sum(x)-1,length(x),.5,lower.tail=FALSE)
N<-rep(100,50000)
mean(sapply(N,f,bias=.57)<.05)

par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
for (n in c(50,500,5000)) {
    out<-list()
    for (i in 1:10000) {
        x<-rbinom(n,1,.57)
        x0<-rbinom(n,1,.5)
        out[[i]]<-c(mean(x),mean(x0))
    }
    x<-do.call('rbind',out)
    plot(density(x[,2]),xlim=c(0,1),yaxt='n',main='',sub='',xlab='Average')
    lines(density(x[,1]),col='red')
    abline(v=.5)
}

#########################################################
tab<-list()
for (N in 1:3) {
    p<-numeric()
    for (i in 1:1000) p[i]<-f(10^N)
    tab[[as.character(N)]]<-c(10^N,mean(p<.05))
}
tab0<-do.call("rbind",tab)

tab<-list()
for (N in 1:3) {
    p<-numeric()
    for (i in 1:1000) p[i]<-f(10^N,bias=0.6)
    tab[[as.character(N)]]<-c(10^N,mean(p<.05))
}
tab1<-do.call("rbind",tab)

#########################################################
N<-sort(runif(5000,10,1000))
p<-list()
for (d in seq(0,.1,by=.01)) p[[as.character(d)]]<-sapply(N,f,bias=.5+d)

par(mgp=c(2,1,0))
plot(NULL,xlim=c(10,1100),ylim=0:1,xlab="# tosses",ylab="Proportion of times that I reject the null")
abline(h=.05,col='gray')
cols<-colorRampPalette(c("blue", "red"))( length(p))
for (i in 8) {
    m0<-loess(p[[i]]<.05 ~ N)
    lines(N,m0$fitted,lty=1,col=cols[i])
    nn<-length(N)
    text(N[nn],m0$fitted[nn],.5+as.numeric(names(p)[i]),pos=4,cex=.85)
}


par(mgp=c(2,1,0))
plot(NULL,xlim=c(10,1100),ylim=0:1,xlab="# tosses",ylab="Proportion of times that I reject the null")
abline(h=.05,col='gray')
cols<-colorRampPalette(c("blue", "red"))( length(p))
for (i in 1:length(p)) {
    m0<-loess(p[[i]]<.05 ~ N)
    lines(N,m0$fitted,lty=1,col=cols[i])
    nn<-length(N)
    text(N[nn],m0$fitted[nn],.5+as.numeric(names(p)[i]),pos=4,cex=.85)
}

par(mgp=c(2,1,0))
plot(NULL,xlim=c(10,1100),ylim=0:1,xlab="# tosses",ylab="Proportion of times that I reject the null")
abline(h=.8,col='gray')
cols<-colorRampPalette(c("blue", "red"))( length(p))
for (i in 1:length(p)) {
    m0<-loess(p[[i]]<.05 ~ N)
    ii<-which.min(abs(m0$fitted-.8)+ifelse(m0$fitted<.8,100,0))
    if (m0$fitted[ii]>.8) {
        nn<-N[ii]
        segments(N[ii],0,N[ii],.8,col=cols[i])
        lty<-1
    } else lty<-2
    lines(N,m0$fitted,lty=lty,col=cols[i])
    nn<-length(N)
    text(N[nn],m0$fitted[nn],.5+as.numeric(names(p)[i]),pos=4,cex=.85)
}



#########################################################
##small effect
N<-sort(runif(500,1,4.5))
p<-sapply(10^N,f,bias=.5+.01)

plot(NULL,xlim=range(N),ylim=0:1,xlab="# tosses",ylab="Proportion of times that I reject the null",xaxt='n')
axis(side=1,at=1:5,labels=c("10","100","1000","10000","100000"))
abline(h=.8,col='gray')
m0<-loess(p<.05 ~ N)
lines(N,m0$fitted,lty=1,col='black')

