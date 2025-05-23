load("pisa_resp.Rdata") #data is at https://www.dropbox.com/s/07i1gmqk082ribt/pisa_resp.Rdata?dl=0
##get model details we will need for simulation
set.seed(1231011)
library(mirt)
resp<-resp[sample(1:nrow(resp),25000),] #take a subsample
m<-mirt(resp,1,"2PL",technical=list(NCYCLES=10000)) #first estimate the item response model
th<-fscores(m) #now estimate abilities

sim<-function(m) {
    co<-coef(m)
    co<-co[-length(co)]
    co<-do.call("rbind",co) #item parameters
    z<-outer(th[,1],co[,1],"*") #outer is a great function. can you see what it is doing? look at help documentatation if it is confusing!
    z<-z+matrix(rep(co[,2],nrow(th)),byrow=TRUE,ncol=nrow(co),nrow=nrow(th))
    pv<-1/(1+exp(-z))
    test<-matrix(runif(nrow(th)*nrow(co)),nrow=nrow(th),ncol=nrow(co)) #a different way of simulating bernoulli random variables
    resp<-test
    resp<-ifelse(pv>test,1,0) #converting the matrix of probabilities to a matrix of ritem responses
    resp
}

f<-function(tau=.1,resp) {
    ss<-rowMeans(resp,na.rm=TRUE)
    z<-resp[ss<tau,]
    cm<-colMeans(z,na.rm=TRUE)
    max(cm)
}
f<-Vectorize(f,'tau')
x.true<-f(tau=seq(.1,.5,by=.1),resp)

L<-list()
for (i in 1:100) {
    z<-sim(m)
    L[[i]]<-f(tau=seq(.1,.5,by=.1),z)
}
x<-do.call("rbind",L)

boxplot(x,ylim=c(0,1))
points(1:5,x.true,pch=19,cex=2,col='red')
