df<-read.csv("Dynarski2003.csv")

df$fatherdec<-ifelse(df$fatherdec=="Father deceased",1,0)

df$before<-ifelse(df$yearsr<=81,1,0)
L<-split(df,paste(df$before,df$fatherdec))
f<-function(x) weighted.mean(x$coll,x$wt88)
lapply(L,f) #compare to table 1


m<-lm(coll~fatherdec*before,df,weights=df$wt88)
summary(m) ##compare to table 2
