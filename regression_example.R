##DGM
N<-1000
x<-rnorm(N)
y<-.3*x+rnorm(N)

##DAM
m<-lm(y~x)
summary(m)


######################################################
##What if we start to make changes to the DGM and ask about their relationship to the empirical data?
##We're going to look at power for a few examples

##how does introducing a third variable (with known correlation to x) disrupt power
library(MASS)
N<-1000
b<-0.00
mat<-list()
for (rho in seq(0,.7,by=.1)) {
    fdr<-numeric()
    for (i in 1:500) {
        xz<-mvrnorm(N,rep(0,2),Sigma=matrix(c(1,rho,rho,1),2,2))
        x<-xz[,1]
        z<-xz[,2]
        y<-b*x+.1*z+rnorm(N)
        m<-lm(y~x)
        fdr[i]<-summary(m)$coef[2,4]<.05
    }
    mat[[as.character(rho)]]<-c(rho,mean(fdr))
}
plot(do.call("rbind",mat),xlab="rho",ylab="false discovery rate",pch=19,cex=2,ylim=c(0,1))
abline(h=0.05,col='gray')



##what about measurement error in our observation of x?
library(MASS)
N<-5000
b<-0.05
mat<-list()
for (s2 in seq(0,1,by=.1)) {
    pow<-numeric()
    for (i in 1:500) {
        x0<-rnorm(N)
        y<-b*x0+rnorm(N)
        x<-x0+rnorm(N,sd=sqrt(s2))
        m<-lm(y~x)
        pow[i]<-summary(m)$coef[2,4]<.05
    }
    mat[[as.character(s2)]]<-c(s2,mean(pow))
}
plot(do.call("rbind",mat),xlab="rho",ylab="pow",pch=19,cex=2,ylim=c(0,1))
abline(h=0.05,col='gray')


##what about measurement error in our observation of y?
library(MASS)
N<-5000
b<-0.15
mat<-list()
for (s2 in seq(0,1,by=.1)) {
    pow<-numeric()
    for (i in 1:500) {
        x0<-rnorm(N)
        y0<-b*x0+rnorm(N)
        y<-y0+rnorm(N,sd=sqrt(s2))
        m<-lm(y~x)
        pow[i]<-summary(m)$coef[2,4]<.05
    }
    mat[[as.character(s2)]]<-c(s2,mean(pow))
}
plot(do.call("rbind",mat),xlab="rho",ylab="discovery rate",pch=19,cex=2,ylim=c(0,1))
abline(h=0.05,col='gray')
