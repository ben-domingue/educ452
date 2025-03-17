x<-read.csv("OK_Payroll.csv")
x<-x[x$teacher & x$year==2020,]
hist(x$experience_total)
N<-nrow(x)

library(MASS)
mod <- fitdistr(x$experience_total, "exponential") ###need to interpret this estimate
##the parameter related to mean and memorylessness
#For an exponential survival distribution, the probability of failure is the same in every time interval, no matter the age of the individual or device. This fact leads to the "memoryless" property of the exponential survival distribution: the age of a subject has no effect on the probability of failure in the next time interval. The exponential may be a good model for the lifetime of a system where parts are replaced as they fail.[7] It may also be useful for modeling survival of living organisms over short intervals. It is not likely to be a good model of the complete lifespan of a living organism.[8] As Efron and Hastie [9] (p. 134) note, "If human lifetimes were exponential there wouldn't be old or young people, just lucky or unlucky ones".



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

##if we replace obs.year with 2010, what will happen to coef(m)[2]? what happens if obs.year is replaced with 2030? 2050?


par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(del,del.c)
hist(del.c, freq = FALSE,breaks=50)
#lines(density(del))
m1<-fitdistr(del.c, "exponential")
curve(dexp(x, rate =m1$estimate), from = 0, col = "red", add = TRUE,lty=2)
m0<-fitdistr(del, "exponential")
curve(dexp(x, rate =m0$estimate), from = 0, col = "black", add = TRUE,lty=2)

##censoring leads to a 'steeper' line
