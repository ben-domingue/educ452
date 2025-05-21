load("pisa2018math_df.Rdata") ##https://www.dropbox.com/s/380xamew0bkrvf9/pisa2018math_df.Rdata?dl=0

##first need to make item response data
ids<-sort(unique(df$id))
L<-split(df,df$item)
resp<-list()
for (i in 1:length(L)) {
    x<-L[[i]]
    test<-ids %in% x$id
    mat.empty<-data.frame(cbind(ids[!test],NA))
    names(mat.empty)<-c("id","resp")
    mat<-rbind(x[,c("id","resp")],mat.empty)
    index<-match(ids,mat$id)
    mat<-mat[index,]
    print(head(mat))
    resp[[i]]<-mat[,2]
}

resp<-data.frame(do.call("cbind",resp))
resp #note sparseness

#save(resp,file="pisa_resp.Rdata"); we'll use this file downstream. available at: https://www.dropbox.com/s/07i1gmqk082ribt/pisa_resp.Rdata?dl=0

load("pisa_resp.Rdata")
##Let's first take a classicical look at this data
par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
dim(resp)
hist(colMeans(resp,na.rm=TRUE),xlim=0:1) ##difficulties
hist(rowMeans(resp,na.rm=TRUE),xlim=0:1) ##proportions of correct responses for a respondent
hist(colMeans(is.na(resp)),xlim=0:1) ####What is going on in this last bit. Hugely important!!

library(mirt)
index<-sample(1:nrow(resp),50000)
resp<-resp[index,]
m<-mirt(resp,1,"2PL")

plot(m,type="trace")

##let's further explore
m
coef(m) #item-level coefficients. what is last one?
##theta estimates
th<-fscores(m)
th
plot(rowMeans(resp,na.rm=TRUE),th)
