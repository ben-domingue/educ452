par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
hist(x$experience_total, freq = FALSE,breaks=50)
curve(dexp(x, rate =mod$estimate), from = 0, col = "red", add = TRUE)

y<-rexp(N,rate=mod$estimate)
qqplot(x$experience_total,y); abline(0,1)

##Implications of censoring & cross-sectional observation
obs.year<-2020
out<-list()
for (i in 1:100) {
    y0<-runif(N,min=1980,max=obs.year)
    y1<-y0+rexp(N,rate=mod$estimate)
    del<-y1-y0
    c1<-coef(fitdistr(del, "exponential"))
    ##
    del.c<-ifelse(y1<=obs.year,y1-y0,obs.year-y0)
    c2<-coef(fitdistr(del.c, "exponential"))
    out[[i]]<-c(c1,c2)
}
z<-do.call("rbind",out)
plot(z)
m<-lm(z[,1]~z[,2])

##if we edit `obs.year<-2020` to use 2010 (while leaving `min=1980`), what will happen to coef(m)[2]? what happens if obs.year is replaced with 2030? 2050?


par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(del,del.c)
hist(del.c, freq = FALSE,breaks=50)
#lines(density(del))
m1<-fitdistr(del.c, "exponential")
curve(dexp(x, rate =m1$estimate), from = 0, col = "red", add = TRUE,lty=2)
m0<-fitdistr(del, "exponential")
curve(dexp(x, rate =m0$estimate), from = 0, col = "black", add = TRUE,lty=2)

##censoring leads to a 'steeper' line
