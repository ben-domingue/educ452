load("/tmp/pisa_resp.Rdata")

library(mirt)
index<-sample(1:nrow(resp),50000)
resp<-resp[index,]
m<-mirt(resp,1,"2PL")
th<-fscores(m)

co<-coef(m)
co<-co[-length(co)]
co<-do.call("rbind",co) #item parameters

z<-outer(th[,1],co[,2],"+")
pv<-1/(1+exp(-z))

resp0<-resp
co0<-co

test<-matrix(runif(nrow(th)*nrow(co)),nrow=nrow(th),ncol=nrow(co))
resp<-ifelse(pv>test,1,0)
plot(colMeans(resp),colMeans(pv))

f<-function(x) {##response/rowmean correlations
    rm<-rowMeans(x,na.rm=TRUE)
    r<-numeric()
    for (i in 1:ncol(x)) r[i]<-cor(rm,x[,i],use='p')
    r
}
r<-f(resp)
r0<-f(resp0) ##this comparison is kind of unconvincing. why?
