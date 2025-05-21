# Getting started ---------------------------------------------------------
## this will be our maiden voyage estimating IRT models
## the main goal is to get oriented with the key output 
## that we get from applications of these models to item response data

## simulating data from the Rasch model
sim<-function(th,b) { # th are abilities, b are difficulties
    np<-length(th)
    ni<-length(b)
    k<-outer(th,b,'-')
    p<-1/(1+exp(-k))
    resp<-p
    for (i in 1:ncol(resp)) resp[,i]<-rbinom(np,1,p[,i])
    data.frame(resp)
}
resp1<-sim(th=rnorm(1000),b=rnorm(25))

## fit the rasch model
library(mirt) # might have to run install.packages("mirt")
m1 <- mirt(resp1, 1, itemtype = "Rasch")
m1 #here is the object containing the estimated rasch model. it contains lots of stuff, we're just seeing a quick summary here

##it has plot methods attached that will generate item response functions (or trace lines, as they are called here)
plot(m1, type = "trace") ## which is the easiest item? the most difficult item?

## we can use the below to get item parameters
coef(m1,simplify=TRUE,IRTpars=TRUE)

## in particular, i would look over this closely vis-a-vis the relevant part of the mirt manual:
## Rasch Only one intercept estimated, and the latent variance of
##      theta is freely estimated. If the data have more than two
##      categories then a partial credit model is used instead (see
##      'gpcm' below).
##           P(x = 1|theta, d) = \frac{1}{1 + exp(-(theta + d))}      

##note the theta+d bit: we are getting -d reported to us given the IRTpars=TRUE argument

## here is a fun way of looking at comparing the estimated icc to empirical data
itemfit(m1, empirical.plot = 3)

##as a comparison
resp2<-resp1
resp2[,1]<-rbinom(nrow(resp2),1,.5)
m2 <- mirt(resp2, 1, itemtype = "Rasch")
itemfit(m2, empirical.plot =1)
