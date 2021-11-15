load("pisa_resp.Rdata") #data is at https://www.dropbox.com/s/07i1gmqk082ribt/pisa_resp.Rdata?dl=0

##we'll first simulate new data
set.seed(1231011)
library(mirt)
resp<-resp[sample(1:nrow(resp),25000),] #take a subsample
m<-mirt(resp,1,"2PL",technical=list(NCYCLES=10000)) #first estimate the item response model
th<-fscores(m) #now estimate abilities

##We'll use item parameters and individual abilities to then reconstruct probabilities of correct responsesco<-coef(m)
co<-coef(m)
co<-co[-length(co)]
co<-do.call("rbind",co) #item parameters
z<-outer(th[,1],co[,1],"*")
z<-z+matrix(rep(co[,2],nrow(th)),byrow=TRUE,ncol=nrow(co),nrow=nrow(th))
pv<-1/(1+exp(-z))

resp0<-resp
co0<-co
    

test<-matrix(runif(nrow(th)*nrow(co)),nrow=nrow(th),ncol=nrow(co))
resp<-ifelse(pv>test,1,0)
plot(colMeans(resp),colMeans(pv))
