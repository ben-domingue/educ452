load("pisa_resp.Rdata")
library(mirt)
index<-sample(1:nrow(resp),10000)
resp<-resp[index,]
m<-mirt(resp,1,"2PL")
th0<-sort(fscores(m)[,1]) #sorting will make it easier for subsequent visualization
co<-coef(m,IRTpars=FALSE) ##note the flag i've set for IRTpars
co<-co[-length(co)]
co0<-do.call("rbind",co) #item parameters

rho<-.5
library(MASS)
mvrnorm(length(th),mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))

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
