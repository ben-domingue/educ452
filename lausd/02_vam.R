load("LA_nice.Rdata") #https://www.dropbox.com/s/vsydexy87z9ot3u/LA_nice.Rdata?dl=0
 
####################################################################
#add prior score
add_scores<-function(x,lag=1) {#feed a df & # lag through function
  names(x)->nms#get names of variables
  #first get prior scores
  x$stud_id_this_year<-paste(x$student_id,x$year,x$grade,x$subject,sep="__")
  #paste names of vars together to get id for this year
  x$stud_id_last_year<-paste(x$student_id,x$year-lag,x$grade-lag,x$subject,sep="__")
  #subtract lag to get lagged grade & year
  tmp<-data.frame(stud_id_last_year=x$stud_id_this_year,scale_score_std_prior=x$scale_score_std)
  #put id & prior together
  col.nm<-paste("scale_score_std_lag",lag,sep="_")
  #rename the prior to match the lag you pulled
  names(tmp)[2]<-col.nm
  x<-merge(x,tmp,all.x=TRUE)
  #
  x[,c(nms,col.nm)]
}
dat<-add_scores(dat)


####################################################################
#random effects
library(lme4)
ma<-dat[dat$subject=="MATHEMATICS",]
mod<-lmer(scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)+(1|teacher_id),ma)
summary(mod)$coeff #compare to table a-1, http://files.eric.ed.gov/fulltext/ED516008.pdf
summary(mod)$varcor #can use this to compute icc
0.287^2/(0.287^2+0.549^2) #=0.21, this is smaller than the 0.297 reported in table a1 of course.
nrow(ranef(mod)[[1]]) #11341 teachers

##fixed effects
library(fixest)
fm<-"scale_score_std~scale_score_std_lag_1+in.title1+ell+join.after.k+factor(grade)+factor(year)"
tmp<-ma[,all.vars(as.formula(fm))]
ma<-ma[rowSums(is.na(tmp))==0,]
mod<-feols(paste(fm,"|teacher_id",sep=""),ma)

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
