x<-read.csv("OK_Payroll.csv") #https://osf.io/v2rgb
x<-x[x$teacher & x$year==2020,]

hist(x$experience_total, freq = FALSE,breaks=50,main="")
N<-nrow(x)

library(MASS)
mod <- fitdistr(x$experience_total, "exponential") ###need to interpret this estimate
##the parameter related to mean and memorylessness
#For an exponential survival distribution, the probability of failure is the same in every time interval, no matter the age of the individual or device. This fact leads to the "memoryless" property of the exponential survival distribution: the age of a subject has no effect on the probability of failure in the next time interval. The exponential may be a good model for the lifetime of a system where parts are replaced as they fail.[7] It may also be useful for modeling survival of living organisms over short intervals. It is not likely to be a good model of the complete lifespan of a living organism.[8] As Efron and Hastie [9] (p. 134) note, "If human lifetimes were exponential there wouldn't be old or young people, just lucky or unlucky ones".

mod
