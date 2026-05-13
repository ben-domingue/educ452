out<-list()
for (mu in seq(0,1,by=.25)) {
    gr<-rbinom(10000,1,.5)
    th<-rnorm(length(gr),mean=mu*gr,sd=1)
    b<-rnorm(50)
    p<-outer(th,b,'-')
    p<-1/(1+exp(-p))
    resp<-p
    for (i in 1:ncol(p)) resp[,i]<-rbinom(nrow(p),1,p[,i])
    resp<-data.frame(resp)
    library(mirt)
    m <- multipleGroup(resp, 1, group = as.character(gr),
                       invariance=c('slopes', 'intercepts','free_means','free_variances')) ##this invariance option is crucial
    out[[as.character(mu)]]<-coef(m)[['1']]$GroupPars
}

cbind(names(out),do.call("rbind",out)[,1])
