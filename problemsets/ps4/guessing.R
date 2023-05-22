load("pisa_resp.Rdata")
library(mirt)
index<-sample(1:nrow(resp),10000)
resp<-resp[index,]
m<-mirt(resp,1,"2PL")
th<-sort(fscores(m)[,1]) #sorting will make it easier for subsequent visualization
co<-coef(m,IRTpars=TRUE)
co<-co[-length(co)]
co<-do.call("rbind",co) #item parameters

##Let's first simulate data the 'standard' way
sim.noguess<-function(th,co) {#this will simulate item responses, estimate irt models, and return abilities based on th and co
    z<-outer(th,co[,2],"-")
    z<-z+matrix(rep(co[,2],length(th)),byrow=TRUE,ncol=nrow(co),nrow=length(th))
    pv<-1/(1+exp(-z))
    test<-matrix(runif(length(th)*nrow(co)),nrow=length(th),ncol=nrow(co)) #a different way of simulating bernoulli random variables
    resp<-ifelse(pv>test,1,0) #converting the matrix of probabilities to a matrix of ritem responses
    resp<-data.frame(resp)
    m<-mirt(resp,1,"2PL")
    fscores(m)
}

##Now let's simulate guessing such that it is a person-level feature with some unique characteristics
sim.guess<-function(th,co,floor=.2,beta=.5) {
    test<-beta*th+rnorm(length(th))
    guesser<-ifelse(test>0,1,0)
    z<-outer(th,co[,2],"-")
    z<-z+matrix(rep(co[,2],length(th)),byrow=TRUE,ncol=nrow(co),nrow=length(th))
    pv<-1/(1+exp(-z))
    for (i in 1:ncol(pv)) pv[,i]<-ifelse(pv[,i]<floor & guesser==1,floor,pv[,i]) ##the key bit
    test<-matrix(runif(length(th)*nrow(co)),nrow=length(th),ncol=nrow(co)) 
    resp<-ifelse(pv>test,1,0) 
    resp<-data.frame(resp)
    m<-mirt(resp,1,"2PL")
    fscores(m)
}

th.ng<-sim.noguess(th,co)
th.g<-sim.guess(th,co)

##let's look for bias in the estimates that don't account for guessing
del0<-th.ng-th
del<-th.g-th
m<-loess(del0~th)
tmp<-cbind(m$x,m$fitted)
plot(tmp,type='l',ylim=c(-1,1)); abline(h=0,col='gray')
m<-loess(del~th)
tmp<-cbind(m$x,m$fitted)
lines(tmp,col='red',lwd=2)

##Let's look at how this might change as a function of beta
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,2,1))
for (beta in c(-1,0,1)) {
    th.g<-sim.guess(th,co,beta=beta)
    del<-th.g-th
    m<-loess(del0~th)
    tmp<-cbind(m$x,m$fitted)
    plot(tmp,type='l',ylim=c(-1,1)); abline(h=0,col='gray')
    m<-loess(del~th)
    tmp<-cbind(m$x,m$fitted)
    lines(tmp,col='red',lwd=2)
    mtext(side=3,line=0,paste0('beta=',beta))
}
