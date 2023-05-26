N<-100
sd.error<-1

tab<-list()
for (b.x in seq(0,1,length.out=4)) {
    print(b.x)
    x<-rnorm(N) #covariate
    t<-rbinom(N,1,.5) #treatment status; note independence from x
    b.t<-sort(runif(250))
    #
    out<-list()
    for (i in 1:length(b.t)) {
        y<-b.x*x+b.t[i]*t+rnorm(N,sd=sd.error)
        m1<-lm(y~t)
        s1<-summary(m1)$coef[2,]
        m2<-lm(y~t+x)
        s2<-summary(m2)$coef[2,]
        m3<-lm(y-x~t)
        s3<-summary(m3)$coef[2,]
        out[[i]]<-rbind(s1,s2,s3)
    }
    z<-list()
    for (i in 1:3) z[[i]]<-sapply(out,function(x) x[i,1])
    rmse<-function(x,y) sqrt(mean((x-y)^2))
    err<-list()
    for (i in 1:3) err[[i]]<-rmse(b.t,z[[i]]) #error of estimate
    ##
    for (i in 1:3) z[[i]]<-sapply(out,function(x) x[i,2])
    prec<-lapply(z,mean) #SE of estimate
    ##
    tab[[as.character(b.x)]]<-cbind(unlist(err),unlist(prec))
}

par(mfcol=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
yl<-c(0,max(unlist(tab)))
cols<-c("black","red","blue")
##
z1<-sapply(tab,function(x) x[,1])
plot(NULL,xlim=0:1,ylim=yl,xlab='bx',ylab='RMSE')
bx<-as.numeric(names(tab))
for (i in 1:3) lines(bx,z1[i,],col=cols[i],lwd=2,lty=i)
##
z1<-sapply(tab,function(x) x[,2])
plot(NULL,xlim=0:1,ylim=yl,xlab='bx',ylab='SE')
for (i in 1:3) lines(bx,z1[i,],col=cols[i],lwd=2,lty=i)
legend("bottom",bty='n',fill=cols,c("simple","cov","diff"))





