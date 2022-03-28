##################################################################################################
##bernoulli
##bernoulli distributions are of interest when we want to simulate dichotomous/binary random variables
##bernoulli distributions are weighted coins. simple! they only have one parameter: the proportion of heads/1s
##the bernoulli is a special case of the binomial distribution which is how we will invoke it
rbinom(1,1,.5) #note that we are calling "rbinom". the second parameter being fixed at 1 is what makes it the special case of the bernoulli distribution
rbinom(10,1,.5) #the first parameter is the number of coin tosses we want
rbinom(100,1,.95) #the last parameter is the proportion of observations that should be 1s

##a totally different way of doing this is to use the uniform distribution.
##basically, we can take a random draw from Unif(0,1) and test it to produce binary outcomes.
##let's test our ability to do this by varying the proportion (p) of 1s:
out<-list()
for (p in seq(.5,.99,by=.01)) {
    x<-runif(100) #the 100 here is controlling the number of coin flips (i.e., the first parameter in the call to rbinom() above)
    y<-ifelse(x>p,0,1)
    out[[as.character(p)]]<-c(p,mean(y))
}
plot(do.call("rbind",out),xlab='p',ylab='proportion 1s')
abline(0,1) #this looks pretty good! how might we decrease variation around the 45 degree line?

##binary outcomes can be both predictors and outcomes. when the latter, they typically get modeled using logistic regression. we'll do that below.

##################################################################################################
##multinomial
##the multinomial distribution is more complicated. it will generate outcomes in different categories. for our purposes, the distribution has one vector-valued parameter: the proportion of responses in each category. 
rmultinom(1,1,rep(1,4)/4) #we'll start by simulating data wherein an observation can be in one of four groups. for the moment, we equally weight the groups. ignore the first two numbers in this line
rmultinom(10,1,rep(1,4)/4) #we can take repeated observations
rmultinom(10,1,c(3/4,c(.25,.25,.25)/3)) #we can also have uneven weights

x<-rmultinom(100,1,c(3/4,c(.25,.25,.25)/3)) #how can we convert this to an indicator about which group the observation is in?
gr<-apply(x,2,function(z) which(z>0)) #this way!
table(gr) #note that most of the observations are in group 1 which is consistent with how we simulated data

##multinomial random variables can be modeled as outcomes, but we'll largely focus on using them to assign, for example, a student to a class (i.e., a student is in a certain group)

##################################################################################################
##normal
rnorm(1,mean=0,sd=1) #hopefully this is starting to look somewhat familiar. 
#you should have a lot of intuition about this. for example, this should all be ok by you
par(mfrow=c(4,1),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
x<-rnorm(1000)
hist(x,xlim=c(-4,4))
x<-rnorm(1000,mean=1)
hist(x,xlim=c(-4,4))
x<-rnorm(1000,sd=1.5)
hist(x,xlim=c(-4,4))
x<-rnorm(1000,mean=-1,sd=.1)
hist(x,xlim=c(-4,4))

##one thing we'll do frequently is draw from the multivariate normal.
##we can use this to simulate, for example, two normally distributed variables with a certain correlation
##let's look at how this works.
library(MASS)
x<-mvrnorm(10000,mu=c(0,0), #mu is the the center of the bivariate normal
           Sigma=matrix(c(1,.5,.5,1),2,2) #this is the variance-covariance matrix
           )
cor(x)
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
hist(x[,1],xlim=c(-4,4))
hist(x[,2],xlim=c(-4,4))
hist(x[x[,2]>2,][,1],xlim=c(-4,4)) #this is just the x1 values where x2>2. what do you expect it to look like?
hist(x[x[,2]<0,][,1],xlim=c(-4,4)) 



##################################################################################################
##now let's put a few things together to see some basic regression examples based on drawing from distributions
##let's first look at a logistic regression example where we are regressing a binary outcome on a normall distributed predictor
beta<-1
x<-rnorm(10000) #this will be the IV
pr<-1/(1+exp(-1*(beta*x))) #the logistic transformation!
y<-rbinom(length(x),1,pr)
m<-glm(y~x,family="binomial")
coef(m)[2] #explore how this output changes as you chance beta

##what if we flip is so that we have a binary predictor of a continuous outcome?
beta<-1
x<-rbinom(10000,1,.5)
y<-beta*x+rnorm(length(x))
m<-lm(y~x)
coef(m)[2] #again, how does this vary as a function of beta?

##what if we have a continuous outcome with a categorical predictor?
beta<-c(.5,1,1,1)
x<-rmultinom(50000,1,rep(1,4)/4) #how can we convert this to an indicator about which group the observation is in?
gr<-apply(x,2,function(z) which(z>0)) #this way!
ng<-max(gr)
L<-list()
for (i in 1:ng) L[[i]]<-ifelse(gr==i,1,0)
xv<-do.call("cbind",L)
pr<-1/(1+exp(-1*(xv %*% matrix(beta,ncol=1)))) #note we made use of matrix multiplicaiton here (%*%)
y<-rbinom(length(x),1,pr[,1])
df<-data.frame(y=y,xv)
fm<-paste("y~0+",paste(names(df)[-1],collapse="+"))
m<-glm(fm,df,family="binomial")
coef(m) #explore this this output changes as you chance beta
