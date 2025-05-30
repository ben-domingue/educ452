simfun<-function(r, #correlation of item difficulty and sensitivity to treatment
                 te, #treatment effect on logit scale
                 N=2000
                 )
{
    pv<-numeric()
    for (ii in 1:100) {
        library(MASS)
        ##person stuff
        treat<-rbinom(1000,1,.5)
        N<-length(treat)
        th<-mvrnorm(N,mu=rep(0,2),Sigma=matrix(c(1,.9,.9,1),2,2))
        th0<-th[,1]
        th<-th[,2]+te*treat
        ##item stuff
        z<-mvrnorm(50,mu=rep(0,2),
                   Sigma=matrix(c(1,r,r,1),2,2)
                   )
        b<-z[,1]
        delta<-z[,2]/3 ##rescaling
        ##
        thmat<-matrix(th,N,50,byrow=FALSE)
        bmat<-matrix(b,N,50,byrow=TRUE)
        delmat<-matrix(delta,N,50,byrow=TRUE)
        for (i in 1:nrow(delmat)) delmat[i,]<-treat[i]*delmat[i,] ##only those in treatment get add'l offset of delta
        ##
        k<- thmat+bmat+delmat
        p<-1/(1+exp(-k))
        resp<-p
        for (i in 1:ncol(resp)) resp[,i]<-rbinom(N,1,p[,i])
        ##
        ss<-rowSums(resp)
        m<-lm(ss~treat*th0)
        pv[ii]<-summary(m)$coef[4,4]
    }
    pv
}

par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(2,1,0))
N<-15000
##should look uniform when r=0
hist(simfun(r=0,te=0,N=N),xlim=0:1,breaks=20,main='')
hist(simfun(r=0,te=.3,N=N),xlim=0:1,breaks=20,main='')
##should have lots of false-posivites when r>0
hist(simfun(r=0.5,te=0,N=N),xlim=0:1,breaks=20,main='')
hist(simfun(r=.5,te=.3,N=N),xlim=0:1,breaks=20,main='')


