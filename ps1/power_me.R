powfun<-function(s2.x,s2.y,
                 N=250,b1=.1,b.treat=.1,
                 nsim=250) {
    pow<-numeric()
    for (i in 1:nsim) {
        x0<-rnorm(N)
        treat<-rbinom(N,1,.5)
        ##
        y<-b.treat*treat+b1*x0+rnorm(N,sd=sqrt(s2.y))
        x<-x0+rnorm(N,sd=sqrt(s2.x))
        m<-lm(y~treat+x)
        pow[i]<-summary(m)$coef[2,4]<.05
    }
    mean(pow)
}

L<-list()
for (s2.y in c(0.1,1,5)) {
    for (s2.x in seq(0,3,by=.5)) {
        L[[paste(s2.y,s2.x)]]<-c(sy=s2.y,sx=s2.x,pow=powfun(N,s2.x=s2.x,s2.y=s2.y))
    }
}

x<-data.frame(do.call("rbind",L))
L<-split(x,x$sy)
plot(NULL,xlim=range(x$sx),ylim=0:1)
cols<-c("black","red","blue")
for (i in 1:length(L)) {
    z<-L[[i]]
    lines(z$sx,z$pow,col=cols[i])
}
legend("topright",bty='n',fill=cols,legend=names(L))
