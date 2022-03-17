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

#save(resp,file="/home/bd/Dropbox/stanford/classes/edu452-spring2022/pisa_resp.Rdata")

dim(resp)
hist(colMeans(resp,na.rm=TRUE))
hist(rowMeans(resp,na.rm=TRUE))
hist(rowMeans(is.na(resp)))


library(mirt)
index<-sample(1:nrow(resp),50000)
resp<-resp[index,]
m<-mirt(resp,1,"2PL")