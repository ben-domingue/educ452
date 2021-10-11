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
b<-0.02
for (rho in c(0,.3,.6)) {
    pow<-numeric()
    for (i in 1:100) {
        xz<-mvrnorm(N,rep(0,2),Sigma=matrix(c(1,rho,rho,1),2,2))
        x<-xz[,1]
        z<-xz[,2]
        y<-b*x+.1*z+rnorm(N)
        m<-lm(y~x)
        pow[i]<-summary(m)$coef[2,4]<.05
    }
    print(rho)
    print(mean(pow))
}

##what about measurement error in our observation of x?
library(MASS)
N<-5000
b<-0.05
for (s2 in c(0,.5,1)) {
    pow<-numeric()
    for (i in 1:500) {
        x0<-rnorm(N)
        y<-b*x0+rnorm(N)
        x<-x0+rnorm(N,sd=sqrt(s2))
        m<-lm(y~x)
        pow[i]<-summary(m)$coef[2,4]<.05
    }
    print(s2)
    print(mean(pow))
}

