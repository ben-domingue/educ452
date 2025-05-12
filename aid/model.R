x<-read.csv("Dynarski2003.csv")


x$before<-ifelse(x$yearsr<=81,1,0)
L<-split(x,paste(x$before,x$fatherdec))
f<-function(x) weighted.mean(x$coll,x$wt88)
lapply(L,f) #compare to table 2

##maybe look at ols error
m<-lm(coll~fatherdec*before,x,weights=x$wt88)
summary(m) ##interaction estimate of 0.256 in table 3; i get 0.18
