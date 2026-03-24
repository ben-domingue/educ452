f<-function(N,delta) {
    N<-round(N/2)
    gr<-c(rep(0,N),rep(1,N))
    x<-rnorm(length(gr),mean=gr*delta)
    df<-data.frame(gr=gr,x=x)
    m<-lm(x~gr,df)
    p<-summary(m)$coef[2,4]
    c(delta,p)
}

N<-sort(runif(5000,min=1,max=4))
L<-list()
library(parallel)
for (delta in seq(.05,.5,by=.05)) { ##delta is Glass's delta: https://en.wikipedia.org/wiki/Effect_size#Glass'_%CE%94
    x<-mclapply(round(10^N),f,delta,mc.cores=10)
    L[[as.character(delta)]]<-do.call("rbind",x)
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
    ii<-which.min(abs(m0$fitted-.8))
    iii<-which.min(abs(m0$fitted-.9))
    tab[[i]]<-c(names(L)[i],round(10^N[ii]),m0$fitted[ii],round(10^N[iii]),m0$fitted[iii])
}
ii<-rev(c(1,length(L)))
legend("topleft",bty='n',legend=names(L)[ii],fill=cols[ii],title="Glass's delta")

z<-do.call("rbind",tab)


