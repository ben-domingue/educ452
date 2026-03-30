powfun<-function(N,b0,b1=.3,s2=.5,
                 nsim=250) {
    pow<-numeric()
    for (i in 1:nsim) {
        x0<-rnorm(N)
        p<-1/(1+exp(-1*(b0+b1*x0)))
        y<-rbinom(N,1,p)
        x<-x0+rnorm(N,sd=sqrt(s2))
        m<-glm(y~x,family='binomial')
        pow[i]<-summary(m)$coef[2,4]<.05
    }
    mean(pow)
}

z<-expand.grid(
    N=c(50,100,200,500,1000),
    b0=c(.5,.9)
)
z$pow<-NA

for (i in 1:nrow(z)) z$pow[i]<-powfun(N=z$N[i],b0=z$b0[i])

L<-split(z,z$b0)
plot(NULL,xlim=range(z$N),ylim=c(0,1))
lines(L[[1]]$N,L[[1]]$pow)
lines(L[[2]]$N,L[[2]]$pow,col='red')
