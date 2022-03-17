############################################################################################################
##A. bernoulli distribution
##a. how good of an estimate of p is the mean? interplay between the magnitude of p and the sample size

p.est<-list()
for (N in c(5,10,50,100,1000)) {
    for (p in seq(0.5,0.99,length.out=25)) {
        m<-numeric()
        for (i in 1:1000) {
            coins<-rbinom(N,1,p)
            m[i]<-mean(coins)
        }
        p.est[[paste(N,p)]]<-c(N,p,mean((m-p)^2))
    }
}

x<-data.frame(do.call("rbind",p.est))
L<-split(x,x[,1])
cols<-c("pink","red","blue","gray","black")
plot(NULL,ylim=c(0,0.1),xlim=range(x[,2]),xlab='p',ylab='mse')
for (i in 1:length(L)) lines(L[[i]][,-1],lwd=2,col=cols[i])
legend("topright",bty='n',fill=cols,names(L))

############################################################################################################
##b. the binomial distribution
ntrial<-15
x<-rbinom(10000,ntrial,.5)
y<-numeric()
for (i in 1:10000) {
    z<-numeric()
    for (j in 1:ntrial) z[j]<-rbinom(1,1,.5)
    y[i]<-sum(z)
}
x<-factor(x,levels=0:ntrial)
y<-factor(y,levels=0:ntrial)
plot(as.numeric(table(x)),as.numeric(table(y))); abline(0,1)

############################################################################################################
##c. the CLT, https://en.wikipedia.org/wiki/Central_limit_theorem
#Which is relevant, N or n, in terms of having the CLT apply?
fun<-function(N,n,p=0.5) { #N is the number of people, n is the number of tosses per person
    x<-rbinom(N,n,p)
    x<-(x-mean(x))/sd(x)
    hist(x,main='',freq=FALSE,yaxt='n')
    xv<-seq(-3,3,length.out=1000)
    lines(xv,dnorm(xv))
    legend("topleft",bty='n',c(paste0("N=",N),paste0("n=",n)))
}
par(mfrow=c(5,5),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(0,4))
for (N in c(10,50,100,1000,5000)) for (n in c(5,25,100,1000,5000)) fun(N,n)

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
plot(m2,z[,1]); abline(0,1)
##What does this tell us?
plot(sd2,z[,2])
##What does this suggest? 

##Note: Analytic derivations of these facts would be preferable for any number of reasons. The goal here is to get our hands dirty with some real simulations but simulations are not always the right tool for the job! 

##b. Now let's look at a bivariate normal distribution
library(MASS)
x<-mvrnorm(500000,mu=rep(0,2),Sigma=matrix(c(1,.5,.5,1),2,2))
##What is the distribution of a conditional look like? So if we look at the probability density when the second variable is 1.5 (or some other value)?
par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1,1))
for (val in c(-2,-1,0,1,1.5,2.5)) {
    x2<-x[abs(x[,2]-val)<.01,]
    plot(density(x2[,1]))
    legend("topleft",bty='n',legend=val)
}

##What do you think? 
##Note the centers of these distributions on the x-axis. What would you need to change in the above to change those values? 
