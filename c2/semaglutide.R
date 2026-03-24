f<-function(delta) {
    r<- 0.5
    N<-25
    library(MASS)
    xx<-mvrnorm(N,mu=rep(0,2),Sigma=matrix(c(1,r,r,1),2,2))
    gr<-c(rep(0,N),rep(1,N))
    df<-data.frame(gr=gr,pre=xx[,1],post=xx[,2])
    df$post<-df$post+delta*df$gr
    m<-lm(post~pre+gr,df)
    m0<-lm(post~gr,df)
    p<-summary(m)$coef[3,4]
    p0<-summary(m0)$coef[2,4]
    c(p,p0)
}

deltas<-seq(0,1,by=.05)
pow<-list()
for (delta in deltas) {
    dd<-rep(delta,500)
    library(parallel)
    p<-mclapply(dd,f,mc.cores=10)
    p<-do.call("rbind",p)
    p<- p<.05
    pow[[as.character(delta)]]<-colMeans(p)
}
pow<-do.call("rbind",pow)

plot(deltas,pow[,1])
lines(deltas,pow[,2],lty=2)
