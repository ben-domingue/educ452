load("pisa_resp.Rdata") #data is at https://www.dropbox.com/s/07i1gmqk082ribt/pisa_resp.Rdata?dl=0

##we'll first simulate new data
library(mirt)
resp<-resp[sample(1:nrow(resp),25000),] #take a subsample
m<-mirt(resp,1,"2PL",technical=list(NCYCLES=10000))
th<-fscores(m)

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
    

mseL<-list()
##let's first look at the quality of estimates relative to true parameters
library(mirt)
resp<-as.data.frame(resp)
names(resp)<-paste0("item",1:ncol(resp))
m<-mirt(resp,1,"2PL",technical=list(NCYCLES=10000))
co<-coef(m)
co<-co[-length(co)]
co<-do.call("rbind",co) #item parameters
plot(co[,2],co0[,2])
mse<-function(x,y) mean((x-y)^2)
mseL$full<-c(mean(is.na(resp)),mse(co[,2],co0[,2]))

##now let's put in the sparseness
resp.na<-resp
for (i in 1:ncol(resp)) resp.na[,i]<-ifelse(is.na(resp0[,i]),NA,resp[,i])
m<-mirt(resp.na,1,"2PL",technical=list(NCYCLES=10000))
co<-coef(m)
co<-co[-length(co)]
co<-do.call("rbind",co) #item parameters
plot(co[,2],co0[,2])
mseL$sparse<-c(mean(is.na(resp.na)),mse(co[,2],co0[,2]))

##let's see how the degree of sparseness affects the mse
for (sparse in seq(0.1,.9,by=.1)) {
    resp.na<-resp
    for (i in 1:ncol(resp)) {
        sp<-rbinom(nrow(resp),1,sparse)
        resp.na[,i]<-ifelse(sp==1,NA,resp[,i])
    }
    rm<-rowMeans(is.na(resp.na))
    resp.na<-resp.na[rm<1,]
    m<-mirt(resp.na,1,"2PL",technical=list(NCYCLES=10000))
    co<-coef(m)
    co<-co[-length(co)]
    co<-do.call("rbind",co) #item parameters
    mseL[[as.character(sparse)]]<-c(sparse,mse(co[,2],co0[,2]))
}
do.call("rbind",mseL)

    
##why is this? could you ask them in a homework problem to explore simulations that get us back to the sparse=.75 mse values being as large as they are empirically?

