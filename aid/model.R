x<-read.csv("Dynarski2003.csv")

x$fatherdec<-ifelse(x$fatherdec=="Father deceased",1,0)

x$before<-ifelse(x$yearsr<=81,1,0)
L<-split(x,paste(x$before,x$fatherdec))
f<-function(x) weighted.mean(x$coll,x$wt88)
lapply(L,f) #compare to table 1


m<-lm(coll~fatherdec*before,x,weights=x$wt88)
summary(m) ##compare to table 2
