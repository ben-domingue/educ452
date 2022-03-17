##DGM
N<-1000
x<-rnorm(N)
y<-.3*x+rnorm(N)

##DAM
m<-lm(y~x)
summary(m)


######################################################
##What if we start to make changes to the DGM and ask about their relationship to the empirical data?
##We're going to look at power (probability that we correctly reject the null hypothesis)and FDR (rate of type 1 error, mistaken rejection of actually true null hypothesis) for a few examples

######################################################
##UNOBSERVED CONFOUNDERS
##how does introducing a third variable (with known correlation to x) disrupt power into the DGM but then omitting it (ie it is unobserved) from the DAM?
library(MASS)
N<-1000
b<-0.00 ##important!
mat<-list()
for (rho in seq(0,.7,by=.1)) {
    fdr<-numeric()
    for (i in 1:500) {
        xz<-mvrnorm(N,rep(0,2),Sigma=matrix(c(1,rho,rho,1),2,2))
        x<-xz[,1]
        z<-xz[,2]
        y<-b*x+.1*z+rnorm(N)  ##note that x does not impact y since b=0!
        m<-lm(y~x)
        fdr[i]<-summary(m)$coef[2,4]<.05
    }
    mat[[as.character(rho)]]<-c(rho,mean(fdr))
}
plot(do.call("rbind",mat),xlab="rho",ylab="false discovery rate",pch=19,cex=2,ylim=c(0,1))
abline(h=0.05,col='gray')
##in the above, we see that FDR increases with rho. that makes sense. z contains information about x due to their correlation and does impact y. if we don't observe z, we might mistakenly think x and y are associated.
##questions to ask: how does the shape of that curve depend upon N? upon the coefficient associated with z in y's assignment? 

######################################################
##MEASUREMENT ERROR IN X
##how does measurement error in X affect statistical power?
library(MASS)
N<-1000
b<-0.1
mat<-list()
for (s2 in seq(0,2,by=.2)) {
    pow<-numeric()
    for (i in 1:100) {
        x0<-rnorm(N)
        y<-b*x0+rnorm(N)
        x<-x0+rnorm(N,sd=sqrt(s2))
        m<-lm(y~x)
        pow[i]<-summary(m)$coef[2,4]<.05
    }
    mat[[as.character(s2)]]<-c(var(x0)/var(x),mean(pow))
}
plot(do.call("rbind",mat),xlab="reliability of x",ylab="statistical power",pch=19,cex=2,ylim=c(0,1))
abline(h=0.05,col='gray')
##as we might have anticipated, power is improved with more reliable measures!.
##you might again wonder how this is impacted by a range of choices we made along the way...
