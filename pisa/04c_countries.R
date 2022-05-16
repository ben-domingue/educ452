load("pisa2018math_df.Rdata") #https://www.dropbox.com/s/380xamew0bkrvf9/pisa2018math_df.Rdata?dl=0

df<-df[df$country %in% c("KOR","TUR","RUS"),]

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

tmp<-df[,c("id","country")]
tmp<-tmp[!duplicated(tmp),]
index<-match(ids,tmp[,1])
ids.country<-tmp[index,]

library(mirt)
m <- multipleGroup(resp, 1, group = ids.country[,2],
                                   invariance=c('slopes', 'intercepts','free_means','free_variances'))

co<-coef(m)
groups<-lapply(co,function(x) x[length(x)][[1]])
groups

###################################################################################
##one concern we might have is country-specific DIF (perhaps due to translation issues). how might we think about this? 
co<-coef(m)[[1]]
co<-do.call("rbind",co[-length(co)])

n.bad<-1 #number of items with DIF
gen.resp<-function(th.pars,co,n.bad=0,N=3000,dif.offset=.2) {
    ##induce dif at random
    if (n.bad>0) {
        index<-sample(1:nrow(co),n.bad)
        for (i in index) co[i,2]<-co[i,2]-dif.offset
    }   
    ##
    th<-rnorm(N,th.pars[1],sqrt(th.pars[2]))
    pv<-outer(th,co[,1],"*")
    pv<-pv+matrix(co[,2],nrow=nrow(pv),ncol=ncol(pv),byrow=TRUE)
    pv<-1/(1+exp(-1*pv))
    resp<-pv
    for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,pv[,i])
    resp
}
g1<-gen.resp(groups[[1]],co)
g2<-gen.resp(groups[[2]],co)
g3<-gen.resp(groups[[1]],co,n.bad=45) #the third group in the simulation will have same ability as first group but 45 of the items will be harder for this group. 

library(mirt)
m <- multipleGroup(data.frame(rbind(g1,g2,g3)), 1, group = as.character(c(rep(1,nrow(g1)),rep(2,nrow(g2)),rep(3,nrow(g3)))),
                   invariance=c('slopes', 'intercepts','free_means','free_variances'))
co<-coef(m)
lapply(co,function(x) x[length(x)][[1]]) 
groups #even if we induce very substantial DIF for group 3, we still end up with a difference that is much larger than the one actually observed between Korea and Turkey
