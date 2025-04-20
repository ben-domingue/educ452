x<-rnorm(1000)
t<-rbinom(1000,1,.5)
y<-.3*x+.15*t+rnorm(1000)
df<-data.frame(x=x,t=t,y=y)
m0<-lm(y~t+x,df)

##bootstrap
est<-numeric()
for (i in 1:100) {
    df2<-df[sample(1:nrow(df),nrow(df),replace=TRUE),]
    m<-lm(y~t+x,df2)
    est[i]<-summary(m)$coef[2,1]
}
s<-summary(m0)$coef[2,]
c(s[1]-1.96*s[2],s[1]+1.96*s[2]) #empirical 95% ci
quantile(est,c(.025,.975)) #bootstrap 95% ci

##randomization inference
est<-numeric()
for (i in 1:100) {
    df2<-df
    df2$t<-sample(df$t,nrow(df),replace=FALSE)
    m<-lm(y~t+x,df2)
    est[i]<-summary(m)$coef[2,1]
}
s<-summary(m0)$coef[2,]
summary(m0)$coef[2,] #empirical estimate
quantile(est,c(.025,.975)) #95% ci around null effect
