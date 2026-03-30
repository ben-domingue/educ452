##################################################################################################
##bernoulli
##bernoulli distributions are of interest when we want to simulate dichotomous/binary random variables
##bernoulli distributions are weighted coins. simple! they only have one parameter: the proportion of heads/1s
##the bernoulli is a special case of the binomial distribution which is how we will invoke it
rbinom(1,1,.5) #note that we are calling "rbinom". the second parameter being fixed at 1 is what makes it the special case of the bernoulli distribution
rbinom(10,1,.5) #the first parameter is the number of coin tosses we want
rbinom(100,1,.95) #the last parameter is the proportion of observations that should be 1s

##binary outcomes can be both predictors and outcomes. when the latter, they typically get modeled using logistic regression. we'll do that below.

##################################################################################################
##multinomial
##the multinomial distribution is more complicated. it will generate outcomes in different categories. for our purposes, the distribution has one vector-valued parameter: the proportion of responses in each category. 
rmultinom(1,1,rep(1,4)/4) #we'll start by simulating data wherein an observation can be in one of four groups. for the moment, we equally weight the groups. ignore the first two numbers in this line
rmultinom(10,1,rep(1,4)/4) #we can take repeated observations
rmultinom(10,1,c(3/4,c(.25,.25,.25)/3)) #we can also have uneven weights

##QQ. Can you write a function that will take the object returned by a call to rmultinom and convert it from the matrix with 1s/0s to a vector telling us which row the 1 was in (where we are depending on the second argument in the call to rmultinom being 1)?

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
x<-rnorm(10000) #this will be the IV

##let's first consider how we can use x to generate a continuous outcome using the standard linear model
b<-1
err.var<-1
y<-b*x+rnorm(length(x),mean=0,sd=err.var)
lm(y~x)

##QQ. Can you generate a binary variable x and use it to produce a continuous outcome y as above. Can you still recover the key regression coefficient b?
##QQ. Can you generate a categorical variable x (using rmultinom) and use it to produce a continuous outcome (that you then analyze with regresion)?
