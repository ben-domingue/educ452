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

resp0<-resp #resp0 contains info on missingness that we'll use throughout

test<-matrix(runif(nrow(th)*nrow(co)),nrow=nrow(th),ncol=nrow(co))
resp<-ifelse(pv>test,1,0)
    
mseL<-list() #this will be the list with a somewhat out-dated name that contains comparisons of truth to estimate
##let's first look at the quality of estimates relative to true parameters
library(mirt)
resp<-as.data.frame(resp)
names(resp)<-paste0("item",1:ncol(resp))
m<-mirt(resp,1,"2PL",technical=list(NCYCLES=10000))
th.est<-fscores(m)

metrics<-function(x,y) { #we're going to use this to compare true and estimated difficulty params
    r1<-mean((x-y)^2)
    r2<-cor(x,y,method="spearman") 
    r3<-mean(x-y)
    c(mse=r1,rank=r2,bias=r3)
}
mseL$full<-c(mean(is.na(resp)),metrics(th[,1],th.est[,1]))

##now let's put in the sparseness
resp.na<-resp #this new dataset will be an indicator of which variables are held out
for (i in 1:ncol(resp)) resp.na[,i]<-ifelse(is.na(resp0[,i]),NA,resp[,i])
m<-mirt(resp.na,1,"2PL",technical=list(NCYCLES=10000))
th.est<-fscores(m)
mseL$sparse<-c(mean(is.na(resp.na)),metrics(th[,1],th.est[,1]))
##
mseL

##let's see how the degree of sparseness affects the mse
for (sparse in seq(0.1,mean(is.na(resp0)),length.out=3)) {
    resp.na<-resp
    for (i in 1:ncol(resp)) {
        sp<-rbinom(nrow(resp),1,sparse)
        resp.na[,i]<-ifelse(sp==1,NA,resp[,i])
    }
    rm<-rowMeans(is.na(resp.na))
    resp.na<-resp.na[rm<1,]
    m<-mirt(resp.na,1,"2PL",technical=list(NCYCLES=10000))
    th.est<-fscores(m)
    mseL[[as.character(sparse)]]<-c(sparse,metrics(th[,1],th.est[,1]))
}
tab<-do.call("rbind",mseL) 
##
tab
