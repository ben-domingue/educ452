f<-function(N,delta) {
    N<-round(N/2)
    x1<-rnorm(N)
    x2<-rnorm(N,mean=delta)
    p<-t.test(x1,x2)$p.value
    ##
    m1<-mean(x1)
    m2<-mean(x2)
    s1<-sd(x1)/sqrt(length(x1))
    s2<-sd(x2)/sqrt(length(x2))
    z<-(m2-m1)/sqrt(s1^2+s2^2)
    pz<-pnorm(abs(z),lower.tail=FALSE)*2
    ##
    c(delta,p,pz)
}

N<-sort(runif(1000,min=10,max=100))
L<-list()
for (delta in seq(.25,1,by=.25)) { 
    x<-lapply(N,f,delta)
    L[[as.character(delta)]]<-do.call("rbind",x)
}

par(mgp=c(2,1,0))
plot(NULL,xlim=range(N),ylim=0:1,xlab="N",ylab="Proportion of times that I reject the null")
abline(h=.8,col='gray')
cols<-colorRampPalette(c("blue", "red"))( length(L))
tab<-list()
for (i in 1:length(L)) {
    x<-L[[i]]
    m0<-loess(x[,2]<.05 ~ N)
    lines(N,m0$fitted,lty=1,col=cols[i])
    m1<-loess(x[,3]<.05 ~ N)
    lines(N,m1$fitted,col=cols[i],lty=2)
}
ii<-rev(c(1,length(L)))
legend("topleft",bty='n',legend=names(L)[ii],fill=cols[ii],title="Glass's delta")
legend("bottomright",bty='n',lty=c(1,2),c("t test","z test"))
