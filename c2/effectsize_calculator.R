f<-function(N,delta) {
    N<-round(N/2)
    gr<-c(rep(0,N),rep(1,N))
    x<-rnorm(length(gr),mean=gr*delta)
    df<-data.frame(gr=gr,x=x)
    m<-lm(x~gr,df)
    p<-summary(m)$coef[2,4]
    c(delta,p)
}
ff<-function(N,delta) {
    p<-numeric()
    library(parallel)
    xx<-mclapply(rep(N,1000),f,delta=delta,mc.cores=10)
    xx<-do.call("rbind",xx)
    p<-xx[,2]
    pow<-sum(p<.05)/length(p)
    pow
}

es<-c('class size reduction'=.3,'therapy/depression'=.22,'antidepressants'=.38,'metformin/fasting glucose'=.87,'oxycodone,/pain'=1.04)
es<-sort(es)


Ns<-numeric()
for (i in 1:length(es)) {
    pow.low<-TRUE
    N<-0
    while (pow.low) {
        if (N<50) N<-N+5 else N<-N+25
        pow<-ff(N,es[i])
        if (pow>.8) pow.low<-FALSE
    }
    Ns[i]<-N
}

cbind(es,Ns)
