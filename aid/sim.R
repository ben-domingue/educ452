m<-lm(coll~fatherdec*before,df,weights=df$wt88)
coef(m)
summary(predict(m))

table(df$coll)
z<-df[,c("fatherdec","before")]
m<-lm(df$coll~z$fatherdec*z$before)
co<-coef(m) #no weights
summary(m)
summary(predict(m))
hist(m$resid)

z$fatherdec<-z$fatherdec+rnorm(nrow(z))
z$before<-z$before+rnorm(nrow(z))


b0<-sort(runif(100,0,3))
out<-list()
co<-c(0,.5,.5,0) #note that i don't think it works with true coefficients (which would be something like (0.43,-0.07,0.04,0.11) in unweighted model)
for (i in 1:length(b0)) {
    k<-b0[i]+co[2]*z$fatherdec+co[3]*z$before
    p<-1/(1+exp(-1*(k)))
    z$y<-rbinom(nrow(z),1,p)
    m<-lm(y~fatherdec*before,z)
    p.lm<-summary(m)$coef[4,4]
    m<-glm(y~fatherdec*before,z,family='binomial')
    p.glm<-summary(m)$coef[4,4]
    out[[i]]<-c(b0[i],p.lm,p.glm,mean(z$y))
}

z<-do.call("rbind",out)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(z[,1],z[,4],xlab='intercept',ylab='prevalence')
m<-loess(z[,2]~z[,1])
plot(z[,1],fitted(m),type='l',col='red',ylim=0:1,ylab='pvalue',xlab='intercept')
m<-loess(z[,3]~z[,1])
lines(z[,1],fitted(m))

