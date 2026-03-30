
############################################################################################################
##C. the normal distribution
##Let's attempt to recover some of the most useful facts about the normal distribution from simulation

##a.i. What proportion of the distribution is more than 1.96 from the mean(=0)?
x<-rnorm(50000,mean=0,sd=1)
sum(abs(x)>1.96)/length(x) #what is this telling us?

##a.iiWhat does the sum of two normal variables look like?
N<-1000
m1<-3
sd1<-2
m2<-rnorm(100,mean=0,sd=3)
sd2<-runif(100,0.1,10)
out<-list()
for (i in 1:length(m2)) {
    x1<-rnorm(N,mean=m1,sd=sd1)
    ##
    x2<-rnorm(N,mean=m2[i],sd=sd2[i])
    ##
    y<-x1+x2
    m<-mean(y)
    s<-sd(y)
    out[[i]]<-c(m,s)
}
z<-do.call("rbind",out)

par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(m2,z[,1],xlab="second mean",ylab="empirical mean"); abline(0,1)
##What does this tell us?
plot(sd2,z[,2],xlab="second sd",ylab="empirical sd")
##What does this suggest? 

##Note: Analytic derivations of these facts would be preferable for any number of reasons. The goal here is to get our hands dirty with some real simulations but simulations are not always the right tool for the job! 

##b. Now let's look at a bivariate normal distribution
library(MASS)
x<-mvrnorm(500000,mu=rep(0,2),Sigma=matrix(c(1,.5,.5,1),2,2))
##What is the distribution of a conditional look like? So if we look at the probability density when the second variable is 1.5 (or some other value)?
par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1,1))
for (val in c(-2,-1,0,1,1.5,2.5)) {
    x2<-x[abs(x[,2]-val)<.01,]
    plot(density(x2[,1]),xlim=c(-6,6))
    legend("topleft",bty='n',legend=val)
}

##What do you think? 
##Note the centers of these distributions on the x-axis. What would you need to change in the above to change those values? 
