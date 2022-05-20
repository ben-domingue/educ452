load("pisa2018math_df.Rdata") ##https://www.dropbox.com/s/380xamew0bkrvf9/pisa2018math_df.Rdata?dl=0

##let's just use a sample of items and countries
items<-unique(df$item)
df<-df[df$item %in% items[1:30],]
countries<-c("POL","JPN","COL","SWE","PER")
df<-df[df$country %in% countries,]
##and a sample of respondents
id<-sample(unique(df$id),max(10000,length(unique(df$id))))
df<-df[df$id %in% id,]
df$id<-paste(df$id,df$country,sep="--") #let's add the country to the id just to have it handy

##first need to make item response data
makeresponse<-function(df) {
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
        resp[[i]]<-mat[,2]
    }
    resp<-data.frame(do.call("cbind",resp))
    for (i in 1:ncol(resp)) resp[,i]<-as.numeric(resp[,i])
    rownames(resp)<-ids
    resp
}
resp<-makeresponse(df)

##now let's look at multiple-group estimates for the empirical data
library(mirt)
cc<-strsplit(rownames(resp),"--")
cc<-sapply(cc,"[",2) #get countries for respondents
m <- multipleGroup(resp, 1, group = cc,
                                   invariance=c('slopes', 'intercepts','free_means','free_variances'))

co<-coef(m)
groups<-sapply(co,function(x) x[length(x)][[1]])
empirical.estimates<-sort(groups[1,])

##Let's now experiment with a simulation wherein we simulate item response data with the empirical structure (e.g., sparseness) and see how well we can do in terms of recovering mean-abilities for the countries we simulate based on sample size
df0<-df

items<-unique(df$item)
items<-data.frame(item=items,diff=rnorm(length(items)),disc=exp(rnorm(length(items),sd=.25)))
df<-merge(df,items)
##let's now add hierchical abilities
country<-unique(df$country)
country<-data.frame(country=country,avg=rnorm(length(country)))
df<-merge(df,country)
df$th<-rnorm(nrow(df),mean=df$avg,sd=1)
k<-df$disc*(df$th-df$diff)
df$p<-1/(1+exp(-k))
df$resp<-rbinom(nrow(df),1,df$p)
resp<-makeresponse(df)
cc<-strsplit(rownames(resp),"--")
cc<-sapply(cc,"[",2) #get countries for respondents
library(mirt)
m <- multipleGroup(resp, 1, group = cc,
                                   invariance=c('slopes', 'intercepts','free_means','free_variances'))
co<-coef(m)
groups<-sapply(co,function(x) x[length(x)][[1]])

sort(groups[1,])
