load("LA_nice_sub.Rdata") 
 


####################################################################
#random effects
library(lme4)
ma<-df[df$subject=="MATHEMATICS",]
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
summary(mod)$coeff #compare to table a-1, http://files.eric.ed.gov/fulltext/ED516008.pdf
summary(mod)$varcor #can use this to compute icc
0.285^2/(0.285^2+0.548^2) #=0.21, this is smaller than the 0.297 reported in table a1 of course.
nrow(ranef(mod)[[1]]) #4011 teachers

##fixed effects
library(fixest)
fm<-"scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)"
tmp<-ma[,all.vars(as.formula(fm))]
ma<-ma[rowSums(is.na(tmp))==0,]
fm2<-as.formula(paste(fm,"|teacher_id",sep=""))
mod<-feols(fm2,ma)

eb<-function(x) {
    aggregate(x$res,list(x$id),sd)->out
    names(out)<-c("id","sd")
    data.frame(table(x$id))->tab
    names(tab)<-c("id","n")
    merge(out,tab,by="id")->out
    out$se<-out$sd^2/out$n
    fe<-x[c("id","fe")]
    fe<-fe[!duplicated(fe$id),]
    merge(fe,out,by=1)->out
    var(out$fe)-mean(out$se,na.rm=TRUE)->v2
    out$shrink<-v2/(v2+out$se)
    out$fe*out$shrink->out$eb
    out
}
ma$resid<-mod$resid
x<-ma[,c("teacher_id","resid")]
names(x)<-c("id","res")
fe<-fixef(mod)$teacher_id
y<-data.frame(id=names(fe),fe=fe)
x<-merge(x,y)
va<-eb(x)[,c("id","eb")]
sd(va$eb,na.rm=TRUE) #0.28, not far from 0.297 in study
