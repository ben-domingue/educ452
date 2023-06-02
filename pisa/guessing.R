##how bad does guessing have to be before it becomes a problem?

g<-.25

out<-list()
for (mu in seq(0,-1.5,length.out=4)) {
    th<-sort(rnorm(10000,mean=mu,sd=1))
    b<-rnorm(50)
    p<-outer(th,b,'-')
    p<-g+(1-g)/(1+exp(-p))
    resp<-p
    for (i in 1:ncol(p)) resp[,i]<-rbinom(nrow(p),1,p[,i])
    resp<-data.frame(resp)
    library(mirt)
    m<-mirt(resp,1,'Rasch')
    th.est<-fscores(m)
    df<-data.frame(th=th,est=th.est[,1])
    df$del<-df$est-df$th
    m<-loess(est~th,df)
    df$fitted<-predict(m)
    out[[as.character(mu)]]<-df
}

cols<-colorRampPalette(c("blue", "red"))( length(out))
par(mfrow=c(1,4),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(out)) {
    plot(NULL,xlim=c(-4,4),ylim=c(-4,4),xlab='true',ylab='est')
    abline(h=0,col='gray'); abline(v=0,col='gray')
    lines(out[[i]]$th,out[[i]]$fitted,type='l',col=cols[i])
    abline(0,1)
    legend("topleft",bty='n',paste0("mu= ",names(out)[i]),cex=2)
}
