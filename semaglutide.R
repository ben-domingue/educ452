f<-function(delta,N=25) {
    r<- 0.5
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


##
out<-list()
for (N in 1:5) {
    pow<-list()
    for (i in 1:1000) pow[[i]]<-f(delta=.5,N=10*N)
    p<-pow<-do.call("rbind",pow)
    out[[paste(2*10*N)]]<-colMeans(p<.05)
}
z<-do.call("rbind",out)

plot(z[,2],z[,1],pch=NA,xlim=0:1,ylim=0:1)
text(z[,2],z[,1],rownames(z))
